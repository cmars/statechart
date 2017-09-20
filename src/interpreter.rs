use std::cmp::{max, Ordering};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use chrono;

use ast::*;

pub struct Interpreter {
    vars: Object,
    events: EventQueue,
    status: Status,
    current_event: Option<Event>,
    current_config: Vec<StateID>,
    next_config: Vec<StateID>,
    exiting_parallels: HashSet<StateID>,
    sender: Box<Sender>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            vars: Object::new(),
            events: EventQueue::new(),
            status: Status::New,
            current_event: None,
            current_config: vec![],
            next_config: vec![],
            exiting_parallels: HashSet::new(),
            sender: Box::new(NopSender),
        }
    }
    pub fn get_var(&self, key: &str) -> Option<&Value> {
        self.vars.get(key)
    }
    pub fn set_var(&mut self, key: &str, value: Value) {
        self.vars.insert(key.to_owned(), value);
    }
    pub fn push_event(&mut self, ev: Event) {
        self.events.push(ev)
    }
    pub fn run(&mut self, ctx: &Context) -> Result<Value, Fault> {
        loop {
            trace!("BEGIN macrostep");
            self.status = self.macrostep(ctx)?;
            trace!("END macrostep: status={:?}", self.status);
            match self.status {
                Status::Done(ref o) => return Ok(o.clone()),
                Status::Blocked => return Err(Fault::BlockedIndefinitely),
                _ => {}
            }
        }
    }
    pub fn step(&mut self, ctx: &Context) -> Result<Status, Fault> {
        trace!("BEGIN macrostep");
        self.status = self.macrostep(ctx)?;
        trace!("END macrostep: status={:?}", self.status);
        Ok(self.status.clone())
    }
    fn macrostep(&mut self, ctx: &Context) -> Result<Status, Fault> {
        match self.status {
            Status::New => {
                let start_t = TransitionBuilder::default()
                    .target_label(Some(ctx.root().node().label().clone()))
                    .build()
                    .unwrap();
                return match self.microstep(ctx, ctx.root(), &start_t) {
                    Ok(status) => {
                        self.complete_macrostep();
                        Ok(status)
                    }
                    Err(e) => Err(e),
                };
            }
            Status::Done(_) => return Ok(self.status.clone()),
            _ => {}
        }

        let mut results = vec![];

        // First follow all eventless transitions that can be taken
        for i in 0..self.current_config.len() {
            let st = match ctx.state_by_id(&self.current_config[i]) {
                None => return Err(Fault::CurrentStateUndefined),
                ref sst => sst.unwrap(),
            };
            if let &State::Final(ref f) = st {
                for on_exit in f.on_exit() {
                    on_exit.actionable().apply(self)?;
                }
                results.push(Status::Done(f.result().outputable().eval(self)));
                continue;
            }
            match self.eventless_transition(ctx, st) {
                Ok(Some(status)) => {
                    results.push(status);
                    continue;
                }
                Ok(None) => {}
                Err(e) => return Err(e),
            }
        }
        if !results.is_empty() {
            self.complete_macrostep();
            return Ok(results.into_iter().fold(Status::Blocked, |agg, val| max(agg, val)));
        }

        // Then follow transitions matched by the next event in the queue.
        if let Some(ref ev) = self.events.pop() {
            for i in 0..self.current_config.len() {
                let st = match ctx.state_by_id(&self.current_config[i]) {
                    None => return Err(Fault::CurrentStateUndefined),
                    ref sst => sst.unwrap(),
                };
                match self.event_transition(ctx, st, ev) {
                    Ok(Some(status)) => {
                        results.push(status);
                        continue;
                    }
                    Ok(None) => {}
                    Err(e) => return Err(e),
                }
            }
        }
        if !results.is_empty() {
            self.complete_macrostep();
            return Ok(results.into_iter().fold(Status::Blocked, |agg, val| max(agg, val)));
        }

        // Finally, enter any non-atomic states in the current configuration.
        for i in 0..self.current_config.len() {
            let st = match ctx.state_by_id(&self.current_config[i]) {
                None => return Err(Fault::CurrentStateUndefined),
                ref sst => sst.unwrap(),
            };
            if let Some(label) = st.node().initial() {
                let status = self.microstep(ctx,
                               st,
                               &TransitionBuilder::default()
                                   .target_label(Some(label.to_owned()))
                                   .build()
                                   .unwrap())?;
                results.push(status);
            } else if let &State::Parallel(_) = st {
                for sub_st in st.substates() {
                    let status = self.microstep(ctx,
                                   st,
                                   &TransitionBuilder::default()
                                       .target_label(Some(sub_st.node().label().to_owned()))
                                       .build()
                                       .unwrap())?;
                    results.push(status);
                }
            }
        }
        self.complete_macrostep();
        Ok(results.into_iter().fold(Status::Blocked, |agg, val| max(agg, val)))
    }
    fn complete_macrostep(&mut self) {
        self.current_config.clear();
        self.current_config.append(&mut self.next_config);
        self.exiting_parallels.clear();
    }
    fn eventless_transition(&mut self, ctx: &Context, st: &State) -> Result<Option<Status>, Fault> {
        let mut sst = Some(st);
        loop {
            sst = match sst {
                Some(st) => {
                    if let Some(n) = st.active_node() {
                        for t in n.transitions() {
                            if t.topics.is_empty() && t.cond.conditional().eval(self) {
                                let status = self.microstep(ctx, st, t)?;
                                return Ok(Some(status));
                            }
                        }
                        match n.parent() {
                            None => None,
                            Some(ref p) => ctx.state_by_id(p),
                        }
                    } else {
                        None
                    }
                }
                None => return Ok(None),
            };
        }
    }
    fn event_transition(&mut self,
                        ctx: &Context,
                        st: &State,
                        ev: &Event)
                        -> Result<Option<Status>, Fault> {
        let mut sst = Some(st);
        loop {
            sst = match sst {
                Some(st) => {
                    if let Some(n) = st.active_node() {
                        for t in n.transitions() {
                            if t.topics.contains(&ev.topic) && t.cond.conditional().eval(self) {
                                let status = self.microstep(ctx, st, t)?;
                                return Ok(Some(status));
                            }
                        }
                        match n.parent() {
                            None => None,
                            Some(ref p) => ctx.state_by_id(p),
                        }
                    } else {
                        None
                    }
                }
                None => return Ok(None),
            };
        }
    }
    fn microstep(&mut self,
                 ctx: &Context,
                 current_st: &State,
                 t: &Transition)
                 -> Result<Status, Fault> {
        trace!("microstep: {:?}", t);
        match t.target_label {
            None => {
                // Execute actions in the current transition and that's it.
                for i in 0..t.actions.len() {
                    t.actions[i].actionable().apply(self)?;
                }
                Ok(Status::Runnable)
            }
            Some(ref target_label) => {
                let target_st = match ctx.state(target_label) {
                    Some(r) => r,
                    None => return Err(Fault::LabelNotFound(target_label.clone())),
                };
                let current_id = current_st.node().id().clone();
                // Execute on_exit for all states we are leaving in this transition.
                let exit_states = Interpreter::exit_states(&current_id, target_st.node().id());
                for id in exit_states {
                    let exit_state = match ctx.state_by_id(&id) {
                        Some(st) => st,
                        None => return Err(Fault::IDNotFound(id.clone())),
                    };
                    if let State::Parallel(_) = *exit_state {
                        if self.exiting_parallels.contains(&id) {
                            // If we've already exited this parallel (in a prior parallel
                            // microstep), we're done.
                            return Ok(Status::TerminatedParallel);
                        } else {
                            self.exiting_parallels.insert(id.clone());
                        }
                    }
                    for on_exit in exit_state.node().on_exit() {
                        on_exit.actionable().apply(self)?;
                    }
                }
                // Execute actions in the current transition.
                for i in 0..t.actions.len() {
                    t.actions[i].actionable().apply(self)?;
                }
                // Execute on_entry for all states we are entering in this transition.
                let entry_states = Interpreter::entry_states(&current_id, target_st.node().id());
                for id in entry_states {
                    let entry_state = match ctx.state_by_id(&id) {
                        Some(st) => st,
                        None => return Err(Fault::IDNotFound(id.clone())),
                    };
                    for on_entry in entry_state.node().on_entry() {
                        on_entry.actionable().apply(self)?;
                    }
                }
                self.next_config.push(target_st.node().id().clone());
                Ok(Status::Runnable)
            }
        }
    }
    pub fn common_ancestor(from: &StateID, to: &StateID) -> StateID {
        let mut common = vec![];
        for i in 0..from.len() {
            if i >= to.len() {
                break;
            }
            if from[i] == to[i] {
                common.push(from[i]);
            } else {
                break;
            }
        }
        common
    }
    pub fn exit_states(from: &StateID, to: &StateID) -> Vec<StateID> {
        let common = Interpreter::common_ancestor(from, to);
        let mut result = vec![];
        let mut i = from.len();
        while i > common.len() {
            let id = from[0..i].to_vec();
            result.push(id);
            i = i - 1;
        }
        result
    }
    pub fn entry_states(from: &StateID, to: &StateID) -> Vec<StateID> {
        let common = Interpreter::common_ancestor(from, to);
        let mut result = vec![];
        for i in common.len()..to.len() {
            let id = to[0..i + 1].to_vec();
            result.push(id);
        }
        result
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Status {
    New,
    Runnable,
    Blocked,
    TerminatedParallel,
    Done(Value),
}

impl Ord for Status {
    fn cmp(&self, other: &Status) -> Ordering {
        self.to_i32().cmp(&other.to_i32())
    }
}

impl PartialOrd for Status {
    fn partial_cmp(&self, other: &Status) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Status {}

impl Status {
    fn to_i32(&self) -> i32 {
        match self {
            &Status::New => 1,
            &Status::Runnable => 2,
            &Status::Blocked => 0,
            &Status::TerminatedParallel => -1,
            &Status::Done(_) => 3,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Fault {
    LabelNotFound(StateLabel),
    IDNotFound(StateID),
    CurrentStateUndefined,
    ActionError(String),
    BlockedIndefinitely,
    UndeliverableSenderTarget(String),
}

pub trait Actionable {
    fn apply(&self, &mut Interpreter) -> Result<(), Fault>;
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Log {
    #[builder(default, setter(into))]
    label: String,
    #[builder(default, setter(into))]
    message: String,
}

#[macro_export]
macro_rules! action_log {
    ($($key:ident: $value:expr),*) => {
        $crate::interpreter::Action::Log($crate::interpreter::LogBuilder::default()$(.$key($value))*.build().unwrap())
    }
}

impl Actionable for Log {
    fn apply(&self, _: &mut Interpreter) -> Result<(), Fault> {
        info!("[{}]{}: {}",
              chrono::prelude::Utc::now().format("%Y-%m-%d %H:%M:%S"),
              self.label,
              self.message);
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Raise {
    #[builder(setter(into))]
    topic: String,
    #[builder(default="Value::Object(HashMap::new())")]
    contents: Value,
}

#[macro_export]
macro_rules! action_raise {
    ($($key:ident: $value:expr),*) => {
        $crate::interpreter::Action::Raise($crate::interpreter::RaiseBuilder::default()$(.$key($value))*.build().unwrap())
    }
}

impl Actionable for Raise {
    fn apply(&self, it: &mut Interpreter) -> Result<(), Fault> {
        trace!("raise {:?}", self);
        it.events.push(Event {
            topic: self.topic.clone(),
            contents: self.contents.clone(),
        });
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Assign {
    #[builder(setter(into))]
    key: String,
    // TODO: make this an Expression
    value: Value,
}

#[macro_export]
macro_rules! action_assign {
    ($($key:ident: $value:expr),*) => {
        $crate::interpreter::Action::Assign($crate::interpreter::AssignBuilder::default()$(.$key($value))*.build().unwrap())
    }
}

impl Actionable for Assign {
    fn apply(&self, it: &mut Interpreter) -> Result<(), Fault> {
        // TODO: partial assignment into objects & lists
        it.vars.insert(self.key.to_owned(), self.value.clone());
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Choose {
    #[builder(default="vec![]")]
    when: Vec<(Condition, Box<Action>)>,
    #[builder(default="None")]
    otherwise: Option<Box<Action>>,
}

impl Actionable for Choose {
    fn apply(&self, it: &mut Interpreter) -> Result<(), Fault> {
        for &(ref cond, ref action) in &self.when {
            if cond.conditional().eval(it) {
                return action.actionable().apply(it);
            }
        }
        if let &Some(ref action) = &self.otherwise {
            return action.actionable().apply(it);
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! choose {
    (@props $c:ident) => {};
    (@props $c:ident, $key:ident: $value:expr) => {
        choose!{@prop $c, $key, $value};
    };
    (@props $c:ident, $key:ident: $value:expr, $($tail:tt)*) => {
        choose!{@prop $c, $key, $value};
        choose!{@props $c $($tail)*};
    };
    (@prop $c:ident, otherwise, $value:expr) => {
        $c.otherwise(Some(Box::new($value)));
    };
    (@prop $c:ident, when, $conds:expr) => {
        $c.when($conds.iter().cloned().map(|(c, a)|{(c, Box::new(a))}).collect());
    };
    ($key:ident: $value:tt) => {{
        let mut c = $crate::interpreter::ChooseBuilder::default();
        choose!{@prop c, $key, $value};
        $crate::interpreter::Action::Choose(c.build().unwrap())
    }};
    ($key:ident: $value:tt, $($tail:tt)*) => {{
        let mut c = $crate::interpreter::ChooseBuilder::default();
        choose!{@prop c, $key, $value};
        choose!{@props c, $($tail)*};
        $crate::interpreter::Action::Choose(c.build().unwrap())
    }};
}

#[derive(Clone)]
pub struct ActionFn {
    f: Rc<Fn(&mut Interpreter) -> Result<(), Fault>>,
}

#[macro_export]
macro_rules! action_fn {
    ($f:expr) => {
        $crate::interpreter::Action::Fn($crate::interpreter::ActionFn::new($f))
    }
}

impl fmt::Debug for ActionFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "(ActionFn)")
    }
}

impl PartialEq for ActionFn {
    fn eq(&self, _: &Self) -> bool {
        return false;
    }
}

impl Actionable for ActionFn {
    fn apply(&self, it: &mut Interpreter) -> Result<(), Fault> {
        (self.f)(it)
    }
}

impl ActionFn {
    pub fn new(f: Rc<Fn(&mut Interpreter) -> Result<(), Fault>>) -> ActionFn {
        ActionFn { f: f }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Action {
    Log(Log),
    Raise(Raise),
    Assign(Assign),
    Choose(Choose),
    Fn(ActionFn),
}

impl Action {
    fn actionable(&self) -> &Actionable {
        match self {
            &Action::Log(ref a) => a,
            &Action::Raise(ref s) => s,
            &Action::Assign(ref a) => a,
            &Action::Choose(ref c) => c,
            &Action::Fn(ref f) => f,
        }
    }
}

pub trait Conditional {
    fn eval(&self, &Interpreter) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
pub struct True;

impl Conditional for True {
    fn eval(&self, _: &Interpreter) -> bool {
        true
    }
}

#[derive(Clone)]
pub struct CondFn {
    f: Rc<Fn(&Interpreter) -> bool>,
}

impl fmt::Debug for CondFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "(CondFn)")
    }
}

impl PartialEq for CondFn {
    fn eq(&self, _: &Self) -> bool {
        return false;
    }
}

impl Conditional for CondFn {
    fn eval(&self, it: &Interpreter) -> bool {
        (self.f)(it)
    }
}

impl CondFn {
    pub fn new(f: Rc<Fn(&Interpreter) -> bool>) -> CondFn {
        CondFn { f: f }
    }
}

#[macro_export]
macro_rules! cond_fn {
    ($f:expr) => {
        $crate::interpreter::Condition::Fn($crate::interpreter::CondFn::new(Rc::new($f)))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    True(True),
    Fn(CondFn), /* TODO:
                 * Expression(Expression), */
}

impl Condition {
    pub fn conditional(&self) -> &Conditional {
        match self {
            &Condition::True(ref c) => c,
            &Condition::Fn(ref f) => f,
        }
    }
}

pub trait Outputable {
    fn eval(&self, &Interpreter) -> Value;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Empty;

impl Outputable for Empty {
    fn eval(&self, _: &Interpreter) -> Value {
        Value::Object(HashMap::new())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Output {
    Empty(Empty),
    ValueOf(ValueOf),
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct ValueOf {
    #[builder(setter(into))]
    key: String,
}

impl Outputable for ValueOf {
    fn eval(&self, it: &Interpreter) -> Value {
        match it.vars.get(&self.key) {
            Some(v) => v.clone(),
            None => Value::None,
        }
    }
}

impl Output {
    pub fn outputable(&self) -> &Outputable {
        match self {
            &Output::Empty(ref o) => o,
            &Output::ValueOf(ref v) => v,
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
    None,
}

impl Value {
    pub fn from_str<S>(s: S) -> Value
        where S: Into<String>
    {
        Value::String(s.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Event {
    topic: String,
    contents: Value,
}

#[derive(Debug)]
pub struct EventQueue(Vec<Event>);

impl EventQueue {
    pub fn new() -> EventQueue {
        EventQueue(vec![])
    }
    pub fn push(&mut self, ev: Event) {
        self.0.insert(0, ev)
    }
    pub fn pop(&mut self) -> Option<Event> {
        self.0.pop()
    }
}

pub trait Sender {
    fn send(&self, target: &str, ev: Event) -> Result<(), Fault>;
}

pub struct NopSender;

impl Sender for NopSender {
    fn send(&self, _: &str, _: Event) -> Result<(), Fault> {
        Ok(())
    }
}

pub struct UndeliverableSender;

impl Sender for UndeliverableSender {
    fn send(&self, target: &str, _: Event) -> Result<(), Fault> {
        Err(Fault::UndeliverableSenderTarget(target.to_owned()))
    }
}
