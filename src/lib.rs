#[macro_use]
extern crate derive_builder;

use std::cell::{RefCell, RefMut, Ref};
use std::collections::HashMap;
use std::io::Error;
use std::rc::{Rc, Weak};

pub type StateID = Vec<usize>;

pub type StateLabel = String;

pub trait Node {
    fn id(&self) -> &StateID;
    fn label(&self) -> &StateLabel;
    fn substate(&self, label: &str) -> Option<Rc<State>>;
    fn parent(&self) -> Weak<State>;
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
    parent: Weak<State>,
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
    fn substate(&self, _: &str) -> Option<Rc<State>> {
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
    }
}

impl Atomic {
    pub fn node(&self) -> &Node {
        self as &Node
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
    #[builder(default="RefCell::new(vec![])")]
    substates: RefCell<Vec<Rc<State>>>,
    #[builder(setter(skip))]
    parent: Weak<State>,
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
    fn substate(&self, label: &str) -> Option<Rc<State>> {
        let ss = self.substates.borrow();
        for i in 0..ss.len() {
            if ss[i].node().label() == label {
                return Some(ss[i].clone());
            }
        }
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
    }
}

impl Compound {
    pub fn node(&self) -> &Node {
        self as &Node
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
    #[builder(default="RefCell::new(vec![])")]
    substates: RefCell<Vec<Rc<State>>>,
    #[builder(setter(skip))]
    parent: Weak<State>,
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
    fn substate(&self, label: &str) -> Option<Rc<State>> {
        let ss = self.substates.borrow();
        for i in 0..ss.len() {
            if ss[i].node().label() == label {
                return Some(ss[i].clone());
            }
        }
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
    }
}

impl Parallel {
    pub fn node(&self) -> &Node {
        self as &Node
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
    parent: Weak<State>,
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
    fn substate(&self, _: &str) -> Option<Rc<State>> {
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
    }
}

impl Final {
    pub fn node(&self) -> &Node {
        self as &Node
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
    fn substates(&self) -> Option<Ref<Vec<Rc<State>>>> {
        match self {
            &State::Atomic(ref a) => None,
            &State::Compound(ref c) => Some(c.substates.borrow()),
            &State::Parallel(ref p) => Some(p.substates.borrow()),
            &State::Final(ref f) => None,
        }
    }
    fn mut_substates(&self) -> Option<RefMut<Vec<Rc<State>>>> {
        match self {
            &State::Atomic(ref a) => None,
            &State::Compound(ref c) => Some(c.substates.borrow_mut()),
            &State::Parallel(ref p) => Some(p.substates.borrow_mut()),
            &State::Final(ref f) => None,
        }
    }
    fn set_parent(&mut self, parent: Weak<State>) {
        match self {
            &mut State::Atomic(ref mut a) => a.parent = parent,
            &mut State::Compound(ref mut c) => c.parent = parent,
            &mut State::Parallel(ref mut p) => p.parent = parent,
            &mut State::Final(ref mut f) => f.parent = parent,
        }
    }
    fn prepend_id(parent: &StateID, target: &mut StateID) {
        for i in parent.len() - 1..0 {
            target.insert(0, parent[i]);
        }
    }
    fn init(st_ref: &mut Rc<State>, id: StateID) {
        {
            let st = Rc::get_mut(st_ref).unwrap();
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
        match st_ref.mut_substates() {
            Some(ref mut ss) => {
                for i in 0..ss.len() {
                    let mut child_id = id.clone();
                    child_id.push(i);
                    {
                        let child_st = Rc::get_mut(&mut ss[i]).unwrap();
                        child_st.set_parent(Rc::downgrade(st_ref));
                    }
                    State::init(&mut ss[i], child_id);
                }
            }
            None => {}
        }
    }
}

#[derive(Debug)]
pub struct Context {
    root: Rc<State>,
    vars: Object,
    events: Vec<Event>,

    state_by_id: HashMap<StateID, Weak<State>>,
    state_by_label: HashMap<StateLabel, Weak<State>>,
}

impl Context {
    pub fn new(root: State) -> Context {
        let mut root_ref = Rc::new(root);
        {
            State::init(&mut root_ref, vec![0])
        }
        let mut ctx = Context {
            root: root_ref.clone(),
            vars: Object::new(),
            events: vec![],
            state_by_id: HashMap::new(),
            state_by_label: HashMap::new(),
        };
        ctx.index(root_ref);
        ctx
    }
    fn index(&mut self, st: Rc<State>) {
        self.state_by_id.insert(st.node().id().clone(), Rc::downgrade(&st));
        self.state_by_label.insert(st.node().label().to_string(), Rc::downgrade(&st));
        if let Some(substates) = st.substates() {
            for i in 0..substates.len() {
                self.index(substates[i].clone());
            }
        }
    }
    pub fn root(&self) -> Rc<State> {
        self.root.clone()
    }
    pub fn state(&self, label: &str) -> Option<Rc<State>> {
        match self.state_by_label.get(&label.to_string()) {
            Some(a) => a.upgrade(),
            None => None,
        }
    }
    pub fn enter_state(&mut self, st: Rc<State>) {
        // pop any prior state up to common ancestor off configuration
        //  - triggering popped states' on_exit
        // push all disjoint ancestors of st not in common onto configuration
        //  - triggering pushed states' on_enter
        // process transitions
    }
}

pub trait Actionable {
    fn apply(&self, &mut Context) -> Result<(), Error>;
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Log {
    #[builder(default, setter(into))]
    label: String,
    #[builder(default, setter(into))]
    message: String,
}

impl Actionable for Log {
    fn apply(&self, _: &mut Context) -> Result<(), Error> {
        println!("[{:?}]{}: {}",
                 std::time::SystemTime::now(),
                 self.label,
                 self.message);
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Send {
    #[builder(setter(into))]
    topic: String,
    #[builder(default="Value::Object(HashMap::new())")]
    contents: Value,
}

impl Actionable for Send {
    fn apply(&self, ctx: &mut Context) -> Result<(), Error> {
        ctx.events.push(Event {
            topic: self.topic.clone(),
            contents: self.contents.clone(),
        });
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Action {
    Log(Log),
    Send(Send),
}

impl Action {
    fn actionable(&self) -> &Actionable {
        match self {
            &Action::Log(ref a) => a,
            &Action::Send(ref s) => s,
        }
    }
}

pub trait Conditional {
    fn eval(&self, &Context) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
pub struct True;

impl Conditional for True {
    fn eval(&self, _: &Context) -> bool {
        true
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    True(True),
}

impl Condition {
    fn conditional(&self) -> &Conditional {
        match self {
            &Condition::True(ref c) => c,
        }
    }
}

pub trait Outputable {
    fn eval(&self, &Context) -> Value;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Empty;

impl Outputable for Empty {
    fn eval(&self, _: &Context) -> Value {
        Value::Object(HashMap::new())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Output {
    Empty(Empty),
}

impl Output {
    fn outputable(&self) -> &Outputable {
        match self {
            &Output::Empty(ref o) => o,
        }
    }
}

pub type Object = HashMap<String, Value>;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    Int(i32),
    String(String),
    List(Vec<Value>),
    Object(HashMap<String, Value>),
}

#[derive(Debug, PartialEq)]
pub struct Event {
    topic: String,
    contents: Value,
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Transition {
    #[builder(default="vec![]")]
    topics: Vec<String>,
    #[builder(default="Condition::True(True)")]
    cond: Condition,
    #[builder(default="vec![]")]
    actions: Vec<Action>,
    #[builder(default="None")]
    target_label: Option<StateLabel>,
}
