use std::collections::{HashMap, HashSet};
use std::iter;
use std::slice::{Iter, IterMut};

use interpreter::{Action, Condition, Output};
use interpreter::{Empty, True};

pub type StateID = Vec<usize>;

pub type StateLabel = String;

pub trait Node {
    fn id(&self) -> &StateID;
    fn label(&self) -> &StateLabel;
    fn substate(&self, label: &str) -> Option<&State>;
    fn initial(&self) -> Option<&StateLabel>;
    fn parent(&self) -> Option<&StateID>;
    fn on_entry(&self) -> &Vec<Action>;
    fn on_exit(&self) -> &Vec<Action>;
}

pub trait ActiveNode: Node {
    fn transitions(&self) -> &Vec<Transition>;
}

#[derive(Debug, Clone, Builder)]
pub struct Atomic {
    #[builder(default="vec![]")]
    id: StateID,
    #[builder(setter(into))]
    label: StateLabel,
    #[builder(default="vec![]")]
    on_entry: Vec<Action>,
    #[builder(default="vec![]")]
    on_exit: Vec<Action>,
    #[builder(default="vec![]")]
    transitions: Vec<Transition>,
    #[builder(setter(skip))]
    parent: Option<StateID>,
}

impl PartialEq for Atomic {
    fn eq(&self, other: &Self) -> bool {
        (&self.id, &self.label, &self.on_entry, &self.on_exit, &self.transitions) ==
        (&other.id, &other.label, &other.on_entry, &other.on_exit, &other.transitions)
    }
}

impl Node for Atomic {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn label(&self) -> &StateLabel {
        &self.label
    }
    fn substate(&self, _: &str) -> Option<&State> {
        None
    }
    fn initial(&self) -> Option<&StateLabel> {
        None
    }
    fn parent(&self) -> Option<&StateID> {
        self.parent.as_ref()
    }
    fn on_entry(&self) -> &Vec<Action> {
        &self.on_entry
    }
    fn on_exit(&self) -> &Vec<Action> {
        &self.on_exit
    }
}

impl ActiveNode for Atomic {
    fn transitions(&self) -> &Vec<Transition> {
        &self.transitions
    }
}

impl Atomic {
    pub fn node(&self) -> &Node {
        self as &Node
    }
    pub fn active_node(&self) -> &ActiveNode {
        self as &ActiveNode
    }
}

#[derive(Debug, Clone, Builder)]
pub struct Compound {
    #[builder(default="vec![]")]
    id: StateID,
    #[builder(setter(into))]
    label: StateLabel,
    #[builder(default="None")]
    initial_label: Option<StateLabel>,
    #[builder(default="vec![]")]
    on_entry: Vec<Action>,
    #[builder(default="vec![]")]
    on_exit: Vec<Action>,
    #[builder(default="vec![]")]
    transitions: Vec<Transition>,
    #[builder(default="vec![]")]
    substates: Vec<State>,
    #[builder(setter(skip))]
    parent: Option<StateID>,
}

impl PartialEq for Compound {
    fn eq(&self, other: &Self) -> bool {
        (&self.id,
         &self.label,
         &self.initial_label,
         &self.on_entry,
         &self.on_exit,
         &self.transitions,
         &self.substates) ==
        (&other.id,
         &other.label,
         &other.initial_label,
         &other.on_entry,
         &other.on_exit,
         &other.transitions,
         &other.substates)
    }
}

impl Node for Compound {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn label(&self) -> &StateLabel {
        &self.label
    }
    fn substate(&self, label: &str) -> Option<&State> {
        for ss in self.substates.iter() {
            if ss.node().label() == label {
                return Some(ss);
            }
        }
        None
    }
    fn initial(&self) -> Option<&StateLabel> {
        match self.initial_label {
            Some(ref l) => Some(l),
            None => {
                for ss in self.substates.iter() {
                    return Some(ss.node().label());
                }
                None
            }
        }
    }
    fn parent(&self) -> Option<&StateID> {
        self.parent.as_ref()
    }
    fn on_entry(&self) -> &Vec<Action> {
        &self.on_entry
    }
    fn on_exit(&self) -> &Vec<Action> {
        &self.on_exit
    }
}
impl ActiveNode for Compound {
    fn transitions(&self) -> &Vec<Transition> {
        &self.transitions
    }
}

impl Compound {
    pub fn node(&self) -> &Node {
        self as &Node
    }
    pub fn active_node(&self) -> &ActiveNode {
        self as &ActiveNode
    }
}

#[derive(Debug, Clone, Builder)]
pub struct Parallel {
    #[builder(default="vec![]")]
    id: StateID,
    #[builder(setter(into))]
    label: StateLabel,
    #[builder(default="vec![]")]
    on_entry: Vec<Action>,
    #[builder(default="vec![]")]
    on_exit: Vec<Action>,
    #[builder(default="vec![]")]
    transitions: Vec<Transition>,
    #[builder(default="vec![]")]
    substates: Vec<State>,
    #[builder(setter(skip))]
    parent: Option<StateID>,
}

impl PartialEq for Parallel {
    fn eq(&self, other: &Self) -> bool {
        (&self.id,
         &self.label,
         &self.on_entry,
         &self.on_exit,
         &self.transitions,
         &self.substates) ==
        (&other.id,
         &other.label,
         &other.on_entry,
         &other.on_exit,
         &other.transitions,
         &other.substates)
    }
}

impl Node for Parallel {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn label(&self) -> &StateLabel {
        &self.label
    }
    fn substate(&self, label: &str) -> Option<&State> {
        for ss in self.substates.iter() {
            if ss.node().label() == label {
                return Some(ss);
            }
        }
        None
    }
    fn initial(&self) -> Option<&StateLabel> {
        None
    }
    fn parent(&self) -> Option<&StateID> {
        self.parent.as_ref()
    }
    fn on_entry(&self) -> &Vec<Action> {
        &self.on_entry
    }
    fn on_exit(&self) -> &Vec<Action> {
        &self.on_exit
    }
}
impl ActiveNode for Parallel {
    fn transitions(&self) -> &Vec<Transition> {
        &self.transitions
    }
}

impl Parallel {
    pub fn node(&self) -> &Node {
        self as &Node
    }
    pub fn active_node(&self) -> &ActiveNode {
        self as &ActiveNode
    }
}

#[derive(Debug, Clone, Builder)]
pub struct Final {
    #[builder(default="vec![]")]
    id: StateID,
    #[builder(setter(into))]
    label: StateLabel,
    #[builder(default="vec![]")]
    on_entry: Vec<Action>,
    #[builder(default="vec![]")]
    on_exit: Vec<Action>,
    #[builder(default="Output::Empty(Empty)")]
    result: Output,
    #[builder(setter(skip))]
    parent: Option<StateID>,
}

impl PartialEq for Final {
    fn eq(&self, other: &Self) -> bool {
        (&self.id, &self.label, &self.on_entry, &self.on_exit, &self.result) ==
        (&other.id, &other.label, &other.on_entry, &other.on_exit, &other.result)
    }
}

impl Node for Final {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn label(&self) -> &StateLabel {
        &self.label
    }
    fn substate(&self, _: &str) -> Option<&State> {
        None
    }
    fn initial(&self) -> Option<&StateLabel> {
        None
    }
    fn parent(&self) -> Option<&StateID> {
        self.parent.as_ref()
    }
    fn on_entry(&self) -> &Vec<Action> {
        &self.on_entry
    }
    fn on_exit(&self) -> &Vec<Action> {
        &self.on_exit
    }
}

impl Final {
    pub fn node(&self) -> &Node {
        self as &Node
    }
    pub fn result(&self) -> &Output {
        &self.result
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum State {
    Atomic(Atomic),
    Compound(Compound),
    Parallel(Parallel),
    Final(Final),
}

impl State {
    pub fn node(&self) -> &Node {
        match self {
            &State::Atomic(ref a) => a.node(),
            &State::Compound(ref c) => c.node(),
            &State::Parallel(ref p) => p.node(),
            &State::Final(ref f) => f.node(),
        }
    }
    pub fn active_node(&self) -> Option<&ActiveNode> {
        match self {
            &State::Atomic(ref a) => Some(a.active_node()),
            &State::Compound(ref c) => Some(c.active_node()),
            &State::Parallel(ref p) => Some(p.active_node()),
            &State::Final(_) => None,
        }
    }
    fn find(&self, id: &StateID) -> Option<&State> {
        self.find_from(id, 0)
    }
    fn find_from(&self, id: &StateID, depth: usize) -> Option<&State> {
        if depth == id.len() {
            return Some(self);
        }
        let ss = match self {
            &State::Atomic(_) => return None,
            &State::Compound(ref c) => &c.substates,
            &State::Parallel(ref p) => &p.substates,
            &State::Final(_) => return None,
        };
        if id[depth] < ss.len() {
            ss[id[depth]].find_from(id, depth + 1)
        } else {
            None
        }
    }
    pub fn allstates(st: &State) -> Vec<&State> {
        iter::once(st).chain(st.substates().flat_map(|ss| State::allstates(ss))).collect()
    }
    pub fn substates(&self) -> Iter<State> {
        match self {
            &State::Atomic(_) => [].iter(),
            &State::Compound(ref c) => c.substates.iter(),
            &State::Parallel(ref p) => p.substates.iter(),
            &State::Final(_) => [].iter(),
        }
    }
    fn substates_mut(&mut self) -> IterMut<State> {
        match self {
            &mut State::Atomic(_) => [].iter_mut(),
            &mut State::Compound(ref mut c) => c.substates.iter_mut(),
            &mut State::Parallel(ref mut p) => p.substates.iter_mut(),
            &mut State::Final(_) => [].iter_mut(),
        }
    }
    fn set_parent(&mut self, parent: StateID) {
        match self {
            &mut State::Atomic(ref mut a) => a.parent = Some(parent),
            &mut State::Compound(ref mut c) => c.parent = Some(parent),
            &mut State::Parallel(ref mut p) => p.parent = Some(parent),
            &mut State::Final(ref mut f) => f.parent = Some(parent),
        }
    }
    fn init(st: &mut State, id: StateID) {
        {
            match st {
                &mut State::Atomic(ref mut a) => {
                    a.id = id;
                    return;
                }
                &mut State::Final(ref mut f) => {
                    f.id = id;
                    return;
                }
                &mut State::Compound(ref mut c) => c.id = id.clone(),
                &mut State::Parallel(ref mut p) => p.id = id.clone(),
            }
        }
        let mut i = 0;
        for ss in st.substates_mut() {
            let mut child_id = id.clone();
            child_id.push(i);
            ss.set_parent(id.clone());
            State::init(ss, child_id);
            i += 1;
        }
    }
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Transition {
    #[builder(default="HashSet::new()")]
    pub topics: HashSet<String>,
    #[builder(default="Condition::True(True)")]
    pub cond: Condition,
    #[builder(default="vec![]")]
    pub actions: Vec<Action>,
    #[builder(default="None")]
    pub target_label: Option<StateLabel>,
}

pub struct Context {
    root: State,
    state_by_label: HashMap<StateLabel, StateID>,
}

impl Context {
    pub fn new(root: State) -> Context {
        let mut root = root;
        State::init(&mut root, vec![]);
        let root = root;
        let mut ctx = Context {
            root: root,
            state_by_label: HashMap::new(),
        };
        for st in State::allstates(&ctx.root) {
            let n = st.node();
            ctx.state_by_label.insert(n.label().to_owned(), n.id().clone());
        }
        ctx
    }
    pub fn root(&self) -> &State {
        &self.root
    }
    pub fn state(&self, label: &str) -> Option<&State> {
        match self.state_by_label.get(label) {
            Some(ref id) => self.state_by_id(id),
            None => None,
        }
    }
    pub fn state_by_id(&self, id: &StateID) -> Option<&State> {
        self.root.find(id)
    }
}

#[macro_export]
macro_rules! state {
    ($label:ident {$($tail:tt)*}) => {{
       let mut stb = $crate::ast::AtomicBuilder::default();
       stb.label(stringify!($label));
       state_props!(stb {$($tail)*});
       $crate::ast::State::Atomic(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! states {
    ($label:ident {$($tail:tt)*}) => {{
        let mut stb = $crate::ast::CompoundBuilder::default();
        stb.label(stringify!($label));
        state_props!(stb {$($tail)*});
        $crate::ast::State::Compound(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! parallel {
    ($label:ident {$($tail:tt)*}) => {{
        let mut stb = $crate::ast::ParallelBuilder::default();
        stb.label(stringify!($label));
        state_props!(stb {$($tail)*});
        $crate::ast::State::Parallel(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! final_state {
    ($label:ident {$($tail:tt)*}) => {{
        let mut stb = $crate::ast::FinalBuilder::default();
        stb.label(stringify!($label));
        state_props!(stb {$($tail)*});
        $crate::ast::State::Final(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! state_props {
    ($stb:ident {$key:ident: $value:expr}) => {
        state_prop!($stb, $key, $value);
    };
    ($stb:ident {$key:ident: $value:expr, $($tail:tt)*}) => {
        state_prop!($stb, $key, $value);
        state_props!($stb {$($tail)*});
    };
    ($stb:ident {}) => {}
}

#[macro_export]
macro_rules! state_prop {
    ($stb:ident, substates, $value:expr) => {
        $stb.substates($value.iter().cloned().collect());
    };
    ($stb:ident, on_entry, $value:expr) => {
        $stb.on_entry($value.iter().cloned().collect());
    };
    ($stb:ident, on_exit, $value:expr) => {
        $stb.on_exit($value.iter().cloned().collect());
    };
    ($stb:ident, transitions, $value:expr) => {
        $stb.transitions($value.iter().cloned().collect());
    };
    ($stb:ident, $key:ident, $value:expr) => {
        $stb.$key($value);
    }
}

#[macro_export]
macro_rules! goto {
    (@props $t:ident, $key:ident: $value:expr) => {
        goto!{@prop $t, $key, $value};
    };
    (@props $t:ident, $key:ident: $value:expr, $($tail:tt)*) => {
        goto!{@prop $t, $key, $value};
        goto!{@props $t, $($tail)*};
    };
    (@prop $t:ident, target, $value:ident) => {
        $t.target_label(Some(stringify!($value).to_owned()));
    };
    (@prop $t:ident, topics, $value:expr) => {
        $t.topics($value.iter().map(|x|{x.to_string()}).collect());
    };
    (@prop $t:ident, actions, $value:expr) => {
        $t.actions($value.iter().cloned().collect());
    };
    (@prop $t:ident, $key:ident, $value:expr) => {
        $t.$key($value);
    };
    ($key:ident: $value:tt) => {{
        let mut t = $crate::ast::TransitionBuilder::default();
        goto!{@prop t, $key, $value};
        t.build().unwrap()
    }};
    ($key:ident: $value:tt, $($tail:tt)*) => {{
        let mut t = $crate::ast::TransitionBuilder::default();
        goto!{@prop t, $key, $value};
        goto!{@props t, $($tail)*};
        t.build().unwrap()
    }};
}
