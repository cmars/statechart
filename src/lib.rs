
use std::collections::HashMap;
use std::io::Error;
use std::marker::Sized;
use std::ops::{Deref, DerefMut};

pub type StateID = String;

pub type StateRef = String;

pub trait State: std::fmt::Debug {
    fn id(&self) -> &StateID;
    fn substate(&self, id: &str) -> Option<&State>;
}

pub trait StateBuilder {
    fn mut_on_entry(&mut self) -> &mut Vec<Box<Action>>;
    fn mut_on_exit(&mut self) -> &mut Vec<Box<Action>>;
    fn push_on_entry(mut self, a: Box<Action>) -> Self
        where Self: Sized
    {
        self.mut_on_entry().push(a);
        self
    }
    fn push_on_exit(mut self, a: Box<Action>) -> Self
        where Self: Sized
    {
        self.mut_on_exit().push(a);
        self
    }
}

pub trait InterimStateBuilder {
    fn mut_transitions(&mut self) -> &mut Vec<Transition>;
    fn push_transition(mut self, t: Transition) -> Self
        where Self: Sized
    {
        self.mut_transitions().push(t);
        self
    }
}

pub trait ParentStateBuilder {
    fn mut_substate(&mut self, id: &str) -> Option<&mut State> {
        let ss = self.mut_substates();
        for i in {
            0..ss.len()
        } {
            if ss[i].id() == id {
                return match ss.get_mut(i) {
                    Some(s) => {
                        let mut sr: &mut State = s.deref_mut();
                        Some(sr)
                    }
                    None => None,
                };
            }
        }
        None
    }
    fn mut_substates(&mut self) -> &mut Vec<Box<State>>;
    fn push_substate(mut self, s: Box<State>) -> Self
        where Self: Sized
    {
        self.mut_substates().push(s);
        self
    }
}

#[derive(Debug)]
pub struct Atomic {
    id: StateID,
    on_entry: Vec<Box<Action>>,
    on_exit: Vec<Box<Action>>,
    transitions: Vec<Transition>,
}

impl State for Atomic {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn substate(&self, _: &str) -> Option<&State> {
        None
    }
}

impl StateBuilder for Atomic {
    fn mut_on_entry(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_entry
    }
    fn mut_on_exit(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_exit
    }
}

impl InterimStateBuilder for Atomic {
    fn mut_transitions(&mut self) -> &mut Vec<Transition> {
        &mut self.transitions
    }
}

impl Atomic {
    pub fn new(id: &str) -> Atomic {
        Atomic {
            id: id.to_string(),
            on_entry: vec![],
            on_exit: vec![],
            transitions: vec![],
        }
    }
}

#[derive(Debug)]
pub struct Compound {
    id: StateID,
    initial_id: Option<StateID>,
    on_entry: Vec<Box<Action>>,
    on_exit: Vec<Box<Action>>,
    transitions: Vec<Transition>,
    substates: Vec<Box<State>>,
}

impl State for Compound {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn substate(&self, id: &str) -> Option<&State> {
        for i in {
            0..self.substates.len()
        } {
            if self.substates[i].id() == id {
                return match self.substates.get(i) {
                    Some(s) => {
                        let sr: &State = s.deref();
                        Some(sr)
                    }
                    None => None,
                };
            }
        }
        None
    }
}

impl StateBuilder for Compound {
    fn mut_on_entry(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_entry
    }
    fn mut_on_exit(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_exit
    }
}

impl InterimStateBuilder for Compound {
    fn mut_transitions(&mut self) -> &mut Vec<Transition> {
        &mut self.transitions
    }
}

impl ParentStateBuilder for Compound {
    fn mut_substates(&mut self) -> &mut Vec<Box<State>> {
        &mut self.substates
    }
}

impl Compound {
    pub fn new(id: &str) -> Compound {
        Compound {
            id: id.to_string(),
            initial_id: None,
            on_entry: vec![],
            on_exit: vec![],
            transitions: vec![],
            substates: vec![],
        }
    }
}

#[derive(Debug)]
pub struct Parallel {
    id: StateID,
    on_entry: Vec<Box<Action>>,
    on_exit: Vec<Box<Action>>,
    transitions: Vec<Transition>,
    substates: Vec<Box<State>>,
}

impl StateBuilder for Parallel {
    fn mut_on_entry(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_entry
    }
    fn mut_on_exit(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_exit
    }
}

impl InterimStateBuilder for Parallel {
    fn mut_transitions(&mut self) -> &mut Vec<Transition> {
        &mut self.transitions
    }
}

impl ParentStateBuilder for Parallel {
    fn mut_substates(&mut self) -> &mut Vec<Box<State>> {
        &mut self.substates
    }
}

impl Parallel {
    pub fn new(id: &str) -> Parallel {
        Parallel {
            id: id.to_string(),
            on_entry: vec![],
            on_exit: vec![],
            transitions: vec![],
            substates: vec![],
        }
    }
}

#[derive(Debug)]
pub struct Final {
    id: StateID,
    on_entry: Vec<Box<Action>>,
    on_exit: Vec<Box<Action>>,
    result: Box<Output>,
}

impl State for Final {
    fn id(&self) -> &StateID {
        &self.id
    }
    fn substate(&self, _: &str) -> Option<&State> {
        None
    }
}

impl StateBuilder for Final {
    fn mut_on_entry(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_entry
    }
    fn mut_on_exit(&mut self) -> &mut Vec<Box<Action>> {
        &mut self.on_exit
    }
}

impl Final {
    pub fn new(id: &str) -> Final {
        Final {
            id: id.to_string(),
            on_entry: vec![],
            on_exit: vec![],
            result: Box::new(EmptyResult),
        }
    }
}

#[derive(Debug)]
pub struct Context<'a> {
    root: &'a State,
    vars: Object,
    events: Vec<Event>,
}

impl<'a> Context<'a> {
    pub fn new(root: &'a State) -> Context<'a> {
        Context {
            root: root,
            vars: Object::new(),
            events: vec![],
        }
    }
}

pub trait Action: std::fmt::Debug {
    fn apply(&self, &mut Context) -> Result<(), Error>;
}

pub trait Condition: std::fmt::Debug {
    fn eval(&self, &Context) -> bool;
}

pub trait Output: std::fmt::Debug {
    fn eval(&self, &Context) -> Value;
}

#[derive(Debug)]
pub struct Log(pub String);

impl Action for Log {
    fn apply(&self, _: &mut Context) -> Result<(), Error> {
        println!("{}", self.0);
        Ok(())
    }
}

#[derive(Debug)]
struct EmptyResult;

impl Output for EmptyResult {
    fn eval(&self, _: &Context) -> Value {
        Value::Object(HashMap::new())
    }
}

#[derive(Debug)]
struct True;

impl Condition for True {
    fn eval(&self, _: &Context) -> bool {
        true
    }
}

pub type Object = HashMap<String, Value>;

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Int(i32),
    String(String),
    List(Vec<Value>),
    Object(HashMap<String, Value>),
}

#[derive(Debug)]
pub struct Event {
    topic: String,
    contents: Value,
}

#[derive(Debug)]
pub struct Transition {
    topics: Vec<String>,
    cond: Box<Condition>,
    actions: Vec<Box<Action>>,
    target_id: Option<StateID>,
}

impl Transition {
    pub fn new() -> Transition {
        Transition {
            topics: vec![],
            cond: Box::new(True),
            actions: vec![],
            target_id: None,
        }
    }
    pub fn push_action(mut self, a: Box<Action>) -> Transition {
        self.actions.push(a);
        self
    }
    pub fn push_topic(mut self, topic: &str) -> Transition {
        self.topics.push(topic.to_string());
        self
    }
    pub fn with_cond(mut self, c: Box<Condition>) -> Transition {
        self.cond = c;
        self
    }
    pub fn with_target(mut self, id: &str) -> Transition {
        self.target_id = Some(id.to_string());
        self
    }
}
