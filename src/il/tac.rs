//! Three-Address Code

use std::{
    borrow::Cow,
    collections::{
        hash_map::{Values, ValuesMut},
        HashMap,
    },
    fmt::{self, Display, Formatter},
    hash::Hash,
    slice::Iter,
};

use crate::{
    ast::{
        typed::{BinOp, CmpOp},
        untyped::Literal,
    },
    builtins::Builtin,
    ext::ordered_hash_map::OrderedHashMap,
    listing::{Listing, Position},
};

pub type TargetSize = isize;

#[derive(Debug)]
pub struct TacProcedure {
    entry_block: BasicBlock,
    blocks: OrderedHashMap<Label, BasicBlock>,
}
impl TacProcedure {
    pub fn new(entry_block: BasicBlock, blocks: OrderedHashMap<Label, BasicBlock>) -> Self {
        Self {
            entry_block,
            blocks,
        }
    }

    pub fn entry_block(&self) -> &BasicBlock {
        &self.entry_block
    }

    pub fn entry_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.entry_block
    }

    pub fn into_blocks(self) -> (BasicBlock, OrderedHashMap<Label, BasicBlock>) {
        (self.entry_block, self.blocks)
    }

    pub fn basic_block(&self, name: &Label) -> &BasicBlock {
        self.blocks.get(name).expect("No such basic block exists")
    }

    pub fn basic_blocks(&self) -> Values<Label, BasicBlock> {
        self.blocks.values()
    }

    pub fn basic_blocks_mut(&mut self) -> ValuesMut<Label, BasicBlock> {
        self.blocks.values_mut()
    }
}
impl Display for TacProcedure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "{}", self.entry_block)?;

        for (name, block) in self.blocks.iter() {
            writeln!(f, "  .{}({}):", name, params_to_string(&block.parameters))?;
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    instructions: Listing<TacInstr>,
    parameters: Vec<Name>,
}
impl BasicBlock {
    pub fn new(parameters: Vec<Name>) -> Self {
        Self {
            instructions: Listing::new(),
            parameters,
        }
    }

    pub fn into_lines_params(self) -> (Listing<TacInstr>, Vec<Name>) {
        (self.instructions, self.parameters)
    }

    pub fn push(&mut self, instruction: TacInstr) {
        self.instructions.push(instruction)
    }

    pub fn current_line_pos(&self) -> Position {
        self.instructions.len() - 1
    }

    pub fn listing(&self) -> &Listing<TacInstr> {
        &self.instructions
    }

    pub fn listing_mut(&mut self) -> &mut Listing<TacInstr> {
        &mut self.instructions
    }

    pub fn parameters(&self) -> &Vec<Name> {
        &self.parameters
    }

    pub fn iter_parameters(&self) -> Iter<Name> {
        self.parameters.iter()
    }
}
impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for instr in self.instructions.iter_instructions() {
            writeln!(f, "      {}", instr)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct TacProgram {
    /// User-defined functions.
    pub functions: HashMap<String, TacProcedure>,
    /// Built-in functions. These functions are not compiled from Python source
    /// code, and are instead directly emitted as assembly code.
    pub builtins: HashMap<String, Builtin>,
    /// The top-level (main) function, containing all code not associated
    /// with another function.
    pub top_level: TacProcedure,
}
impl TacProgram {
    pub fn new(top_level: TacProcedure) -> Self {
        Self {
            functions: HashMap::new(),
            builtins: HashMap::new(),
            top_level,
        }
    }
}
impl Display for TacProgram {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "main:")?;
        writeln!(f, "{}", self.top_level)?;
        for (name, body) in self.functions.iter() {
            writeln!(f, "{name}:")?;
            writeln!(f, "{body}")?;
        }

        Ok(())
    }
}

impl Listing<TacInstr> {
    pub fn is_used_after(&self, name: &Name, position: Position) -> bool {
        // We can probably always subtract 1 from `len` here
        if position >= self.len() {
            return false;
        }

        self.iter_lines()
            .skip_while(|(l, _)| l < &position)
            .any(|(_, instr)| instr.reads_from_name(name))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    name: String,
    subscript: usize,
}
impl Label {
    pub fn new(name: String, subscript: usize) -> Self {
        Self { name, subscript }
    }
}
impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.subscript)
    }
}

/// A single TAC instruction.
#[derive(Debug, Clone)]
pub enum TacInstr {
    /// Assign a value to a name.
    Assign(Name, Value),
    /// Perform a binary operation.
    Bin(Name, BinOp, Value, Value),
    /// Jump to a label, optionally defining a list of variables that will be
    /// read following the jump.
    ///
    /// This is only useful for backwards jumps,
    /// because it means that the lifetime of these variables should be extended
    /// up to the point at which the jump occurs. For instance:
    /// ```
    /// target:
    ///     %a = x^1 * 2
    ///     // Normally the lifetime of `x^1` would have ended here, and the
    ///     // `%b` variable declared below could reuse the register used by `x^1`.
    ///     %b = %a + %a
    ///     // But as a result of this `goto`, `x^1` will be read again when the
    ///     // multiplication by two is performed again after jumping. The `goto`
    ///     // instruction indicates this by including `x^1` in its parameters.
    ///     goto target(x^1)
    /// ```
    ///
    Goto(Label, Vec<Name>),
    /// Jump if a value is true.
    If(Value, Label, Label, Vec<Name>),
    /// Jump if a comparison evaluates to true.
    IfCmp(Value, CmpOp, Value, Label, Label, Vec<Name>),
    /// Call a function, passing `n` parameters.
    Call(Option<Name>, String, Vec<Value>),
    /// Return a value from a function body.
    Return(Option<Value>),
}
impl TacInstr {
    pub fn reads_from_name(&self, name: &Name) -> bool {
        match self {
            Self::Assign(_, value) => Self::is_usage_of(name, value),
            Self::Bin(_, _, lhs, rhs) => {
                Self::is_usage_of(name, lhs) || Self::is_usage_of(name, rhs)
            }
            Self::Call(_, _, values) => values.iter().any(|v| Self::is_usage_of(name, v)),
            Self::Goto(_, names) => names.iter().any(|n| n == name),
            Self::If(value, _, _, names) => {
                Self::is_usage_of(name, value) || names.iter().any(|n| n == name)
            }
            Self::IfCmp(lhs, _, rhs, _, _, names) => {
                Self::is_usage_of(name, lhs)
                    || Self::is_usage_of(name, rhs)
                    || names.iter().any(|n| n == name)
            }
            Self::Return(None) => false,
            Self::Return(Some(value)) => Self::is_usage_of(name, value),
        }
    }

    pub fn reads(&self) -> Vec<Name> {
        fn collect<'a, I: IntoIterator<Item = &'a Value>>(values: I) -> Vec<Name> {
            let mut result = vec![];
            for val in values {
                result.extend(val.as_name().cloned().into_iter());
            }
            result
        }

        match self {
            Self::Assign(_, v) => collect([v]),
            Self::Bin(_, _, lhs, rhs) => collect([lhs, rhs]),
            Self::Goto(_, names) => names.to_vec(),
            Self::If(v, _, _, names) => names.iter().cloned().chain(collect([v])).collect(),
            Self::IfCmp(lhs, _, rhs, _, _, names) => {
                names.iter().cloned().chain(collect([lhs, rhs])).collect()
            }
            Self::Call(_, _, vs) => collect(vs),
            Self::Return(v) => collect(v),
        }
    }

    pub fn write(&self) -> Option<&Name> {
        match self {
            Self::Assign(t, _) => Some(t),
            Self::Bin(t, _, _, _) => Some(t),
            Self::Goto(_, _) => None,
            Self::If(_, _, _, _) => None,
            Self::IfCmp(_, _, _, _, _, _) => None,
            Self::Call(t, _, _) => t.as_ref(),
            Self::Return(_) => None,
        }
    }

    pub fn may_replace(&self, name: &Name) -> bool {
        match self {
            // Names referenced by a jump target may not be replaced right away.
            Self::Goto(_, names) => names.iter().all(|n| n != name),
            Self::If(_, _, _, names) => names.iter().all(|n| n != name),
            Self::IfCmp(_, _, _, _, _, names) => names.iter().all(|n| n != name),
            _ => true,
        }
    }

    /// Replace all occurrences of a name in this instruction with a value.
    pub fn replace(&mut self, src: &Name, dest: Cow<Value>) {
        fn try_replace(tgt: &mut Value, src: &Name, dest: Cow<Value>) {
            if let Value::Name(name) = tgt {
                if name == src {
                    *tgt = dest.into_owned();
                }
            }
        }

        fn replace_in(names: &mut Vec<Name>, src: &Name, dest: Cow<Value>) {
            match dest.as_ref() {
                Value::Const(_) => {
                    // If we replace a name with a constant, it effectively results in that name
                    // no longer being read by the jump target, so we can just remove its reference.
                    names.retain(|n| n != src);
                }
                Value::Name(dst_name) => {
                    if names.iter().any(|n| n == src) {
                        names.retain(|n| n != src);
                        // If we already read from the destination name, we don't need to add it again.
                        if !names.iter().any(|n| n == dst_name) {
                            names.push(dst_name.clone());
                        }
                    }
                }
            }
        }

        match self {
            Self::Assign(_, value) => try_replace(value, src, dest),
            Self::Bin(_, _, lhs, rhs) => {
                try_replace(lhs, src, dest.clone());
                try_replace(rhs, src, dest);
            }
            Self::Call(_, _, args) => {
                for arg in args.iter_mut() {
                    try_replace(arg, src, dest.clone())
                }
            }
            Self::Goto(_, names) => replace_in(names, src, dest),
            Self::If(value, _, _, names) => {
                try_replace(value, src, dest.clone());
                replace_in(names, src, dest);
            }
            Self::IfCmp(lhs, _, rhs, _, _, names) => {
                try_replace(lhs, src, dest.clone());
                try_replace(rhs, src, dest.clone());
                replace_in(names, src, dest);
            }
            Self::Return(None) => (),
            Self::Return(Some(value)) => try_replace(value, src, dest),
        }
    }

    pub fn as_assign(&self) -> Option<(&Name, &Value)> {
        match self {
            Self::Assign(name, value) => Some((name, value)),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<(Option<&Name>, &String, &Vec<Value>)> {
        match self {
            Self::Call(name, function, params) => Some((name.as_ref(), function, params)),
            _ => None,
        }
    }

    pub fn replace_assign(&mut self, src: &Name, dest: Cow<Name>) {
        fn try_replace(tgt: &mut Name, src: &Name, dest: Cow<Name>) {
            if tgt == src {
                *tgt = dest.into_owned();
            }
        }
        match self {
            Self::Assign(tgt, _) => try_replace(tgt, src, dest),
            Self::Bin(tgt, _, _, _) => try_replace(tgt, src, dest),
            Self::Goto(_, _) => (),
            Self::If(_, _, _, _) => (),
            Self::IfCmp(_, _, _, _, _, _) => (),
            Self::Call(Some(tgt), _, _) => try_replace(tgt, src, dest),
            Self::Call(_, _, _) => (),
            Self::Return(_) => (),
        }
    }

    fn is_usage_of(name: &Name, value: &Value) -> bool {
        match value {
            Value::Name(n) => name == n,
            Value::Const(_) => false,
        }
    }
}
impl Display for TacInstr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Assign(target, value) => write!(f, "{} = {}", target, value),
            Self::Bin(target, op, lhs, rhs) => {
                write!(f, "{} = {} {} {}", target, lhs, op, rhs)
            }
            Self::Call(Some(name), tgt, params) => {
                write!(f, "{} = call {} ({})", name, tgt, params_to_string(params),)
            }
            Self::Call(None, tgt, params) => {
                write!(f, "call {} ({})", tgt, params_to_string(params))
            }
            Self::Goto(label, names) if names.is_empty() => write!(f, "goto {}", label),
            Self::Goto(label, names) => write!(f, "goto {} ({})", label, params_to_string(names)),
            Self::If(value, true_lbl, false_lbl, names) => {
                write!(
                    f,
                    "if {} goto {} else goto {} ({})",
                    value,
                    true_lbl,
                    false_lbl,
                    params_to_string(names)
                )
            }
            Self::IfCmp(lhs, op, rhs, true_lbl, false_lbl, names) => {
                write!(
                    f,
                    "if {} {} {} goto {} else goto {} ({})",
                    lhs,
                    op,
                    rhs,
                    true_lbl,
                    false_lbl,
                    params_to_string(names)
                )
            }
            Self::Return(None) => f.write_str("return"),
            Self::Return(Some(value)) => write!(f, "return {}", value),
        }
    }
}

/// A TAC name. Names are symbolic addresses and may represent variables in the original source
/// code, or intermediate values of complex computations that have been broken down.
///
/// Each name is only ever assigned to once. If a variable in the source code is assigned to
/// multiple times, the IL generator will subscript each 'assignment lifetime'.
/// In other words, given the following source code:
/// ```
/// x = 10
/// y = 42 + x
/// x = y - 2
/// ```
/// The following TAC is generated:
/// ```
/// x^1 = 10
/// y^1 = 42 + x^1
/// x^2 = y^1 - 2
/// ```
/// This makes it easier to reason about the lifetime of each variable, which in turn allows us
/// to better optimise the code.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Name {
    /// A subscripted variable.
    Sub(Variable),
    /// A generated, temporary name.
    Temp(TempVariable),
}
impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Name::Sub(sub) => sub.fmt(f),
            Name::Temp(temp) => write!(f, "%{}", temp.id()),
        }
    }
}

/// A subscripted variable.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Variable {
    pub name: String,
    pub subscript: usize,
}
impl Variable {
    pub fn new(name: String, subscript: usize) -> Self {
        Self { name, subscript }
    }
}
impl Display for Variable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}^{}", self.name, self.subscript)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct TempVariable(usize);
impl TempVariable {
    pub fn new(index: usize) -> TempVariable {
        Self(index)
    }

    pub fn id(&self) -> usize {
        self.0
    }
}
impl Display for TempVariable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// A TAC value. Values can be constants, or references to names that were
/// defined earlier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A constant, specified in the target size of the destination platform.
    Const(TargetSize),
    /// A name, representing either a temporary name or a variable in the source program.
    Name(Name),
}
impl Value {
    fn as_name(&self) -> Option<&Name> {
        match self {
            Value::Const(_) => None,
            Value::Name(n) => Some(n),
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::Const(lit) => write!(f, "{}", lit),
            Value::Name(name) => write!(f, "{}", name),
        }
    }
}
impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Integer(i) => Value::Const(i as TargetSize),
            Literal::Boolean(b) => Value::Const(b as TargetSize),
            Literal::String(_) => todo!("Convert `str` to TAC value."),
            Literal::None => todo!("Convert `None` to TAC value"),
        }
    }
}

pub struct LineNumberIter<I, F> {
    iter: I,
    f: F,
}
impl<A, B, I, F> Iterator for LineNumberIter<I, F>
where
    I: Iterator<Item = (Position, A)>,
    F: FnMut(A) -> Option<B>,
{
    type Item = (Position, B);

    fn next(&mut self) -> Option<(Position, B)> {
        self.iter
            .find_map(|(line, a)| (self.f)(a).map(|x| (line, x)))
    }
}

pub trait MatchInstruction<A, I> {
    fn match_instruction<P, B>(self, pred: P) -> LineNumberIter<I, P>
    where
        P: Fn(A) -> Option<B>;
}
impl<'a, I> MatchInstruction<&'a TacInstr, I> for I
where
    I: Iterator<Item = (Position, &'a TacInstr)>,
{
    fn match_instruction<P, B>(self, pred: P) -> LineNumberIter<I, P>
    where
        P: Fn(&'a TacInstr) -> Option<B>,
    {
        LineNumberIter {
            iter: self,
            f: pred,
        }
    }
}

fn params_to_string<P: ToString>(params: &[P]) -> String {
    params
        .iter()
        .map(|p| p.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}
