#[macro_use]
extern crate derive_builder;

use std::cell::{RefCell, RefMut, Ref};
use std::collections::HashMap;
use std::io::Error;
use std::rc::{Rc, Weak};

pub type StateID = Vec<usize>;

pub type StateLabel = String;

#[derive(Debug, PartialEq, Clone, Builder)]
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
}

#[derive(Debug, PartialEq, Clone, Builder)]
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
}

#[derive(Debug, PartialEq, Clone, Builder)]
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
}

#[derive(Debug, PartialEq, Clone, Builder)]
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum State {
    Atomic(Atomic),
    Compound(Compound),
    Parallel(Parallel),
    Final(Final),
}

impl State {
    pub fn id(&self) -> &StateID {
        match self {
            &State::Atomic(ref a) => &a.id,
            &State::Compound(ref c) => &c.id,
            &State::Parallel(ref p) => &p.id,
            &State::Final(ref f) => &f.id,
        }
    }
    pub fn label(&self) -> &StateLabel {
        match self {
            &State::Atomic(ref a) => &a.label,
            &State::Compound(ref c) => &c.label,
            &State::Parallel(ref p) => &p.label,
            &State::Final(ref f) => &f.label,
        }
    }
    pub fn substate(&self, label: &str) -> Option<Rc<State>> {
        let ss = match self {
            &State::Atomic(ref a) => return None,
            &State::Compound(ref c) => c.substates.borrow(),
            &State::Parallel(ref p) => p.substates.borrow(),
            &State::Final(ref f) => return None,
        };
        for i in 0..ss.len() {
            if ss[i].label() == label {
                return Some(ss[i].clone());
            }
        }
        None
    }
    fn substates(&self) -> Option<Ref<Vec<Rc<State>>>> {
        match self {
            &State::Atomic(ref a) => None,
            &State::Compound(ref c) => Some(c.substates.borrow()),
            &State::Parallel(ref p) => Some(p.substates.borrow()),
            &State::Final(ref f) => None,
        }
    }
    fn mut_substates(&mut self) -> Option<RefMut<Vec<Rc<State>>>> {
        match self {
            &mut State::Atomic(ref mut a) => None,
            &mut State::Compound(ref mut c) => Some(c.substates.borrow_mut()),
            &mut State::Parallel(ref mut p) => Some(p.substates.borrow_mut()),
            &mut State::Final(ref mut f) => None,
        }
    }
    pub fn set_root(&mut self) {
        self.set_id(vec![0]);
    }
    fn set_id(&mut self, new_id: StateID) {
        let id = {
            match self {
                &mut State::Atomic(ref mut a) => {
                    a.id = new_id;
                    return;
                }
                &mut State::Compound(ref mut c) => c.id = new_id.clone(),
                &mut State::Parallel(ref mut p) => p.id = new_id.clone(),
                &mut State::Final(ref mut f) => {
                    f.id = new_id;
                    return;
                }
            }
        };
        match self.mut_substates() {
            Some(ref mut ss) => {
                for i in 0..ss.len() {
                    let mut child_id = new_id.clone();
                    child_id.push(i);
                    Rc::get_mut(&mut ss[i]).unwrap().set_id(child_id);
                }
            }
            None => {}
        }
    }
    fn prepend_id(parent: &StateID, target: &mut StateID) {
        for i in parent.len() - 1..0 {
            target.insert(0, parent[i]);
        }
    }
}

#[derive(Debug)]
pub struct Context {
    root: Rc<State>,
    vars: Object,
    events: Vec<Event>,

    states: HashMap<StateLabel, Weak<State>>,
}

impl Context {
    pub fn new(root: State) -> Context {
        let mut root_ref = Rc::new(root);
        {
            Rc::get_mut(&mut root_ref).unwrap().set_root()
        }
        let mut ctx = Context {
            root: root_ref.clone(),
            vars: Object::new(),
            events: vec![],
            states: HashMap::new(),
        };
        ctx.index(root_ref);
        ctx
    }
    fn index(&mut self, st: Rc<State>) {
        self.states.insert(st.label().to_string(), Rc::downgrade(&st));
        if let Some(substates) = st.substates() {
            for i in 0..substates.len() {
                self.index(substates[i].clone());
            }
        }
    }
    pub fn state(&self, label: &str) -> Option<Rc<State>> {
        match self.states.get(&label.to_string()) {
            Some(a) => a.upgrade(),
            None => None,
        }
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
