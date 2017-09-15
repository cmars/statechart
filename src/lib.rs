#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate log;

use std::cell::{RefCell, RefMut, Ref};
use std::cmp::{max, Ordering};
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

extern crate chrono;

pub type StateID = Vec<usize>;

pub type StateLabel = String;

pub trait Node {
    fn id(&self) -> &StateID;
    fn label(&self) -> &StateLabel;
    fn substate(&self, label: &str) -> Option<Rc<State>>;
    fn initial(&self) -> Option<StateLabel>;
    fn parent(&self) -> Weak<State>;
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
    fn initial(&self) -> Option<StateLabel> {
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
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
    fn initial(&self) -> Option<StateLabel> {
        match self.initial_label {
            Some(ref l) => Some(l.to_string()),
            None => {
                let ss = self.substates.borrow();
                if ss.len() > 0 {
                    Some(ss[0].node().label().to_string())
                } else {
                    None
                }
            }
        }
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
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
    fn initial(&self) -> Option<StateLabel> {
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
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
    fn initial(&self) -> Option<StateLabel> {
        None
    }
    fn parent(&self) -> Weak<State> {
        self.parent.clone()
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
    fn substates(&self) -> Option<Ref<Vec<Rc<State>>>> {
        match self {
            &State::Atomic(_) => None,
            &State::Compound(ref c) => Some(c.substates.borrow()),
            &State::Parallel(ref p) => Some(p.substates.borrow()),
            &State::Final(_) => None,
        }
    }
    fn mut_substates(&self) -> Option<RefMut<Vec<Rc<State>>>> {
        match self {
            &State::Atomic(_) => None,
            &State::Compound(ref c) => Some(c.substates.borrow_mut()),
            &State::Parallel(ref p) => Some(p.substates.borrow_mut()),
            &State::Final(_) => None,
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
    events: EventQueue,

    state_by_id: HashMap<StateID, Weak<State>>,
    state_by_label: HashMap<StateLabel, Weak<State>>,

    status: Status,
    current_event: Option<Event>,
    current_config: Vec<Weak<State>>,
    next_config: Vec<Weak<State>>,
    exiting_parallels: HashSet<StateID>,
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
            events: EventQueue::new(),
            state_by_id: HashMap::new(),
            state_by_label: HashMap::new(),
            status: Status::New,
            current_event: None,
            current_config: vec![],
            next_config: vec![],
            exiting_parallels: HashSet::new(),
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
    pub fn state_by_id(&self, id: &StateID) -> Option<Rc<State>> {
        match self.state_by_id.get(id) {
            Some(a) => a.upgrade(),
            None => None,
        }
    }
    pub fn get_var(&self, key: &str) -> Option<&Value> {
        self.vars.get(key)
    }
    pub fn set_var(&mut self, key: &str, value: Value) {
        self.vars.insert(key.to_string(), value);
    }
    pub fn run(&mut self) -> Result<Value, Fault> {
        loop {
            trace!("BEGIN macrostep");
            self.status = self.macrostep()?;
            trace!("END macrostep: status={:?}", self.status);
            match self.status {
                Status::Done(ref o) => return Ok(o.clone()),
                Status::Blocked => return Err(Fault::BlockedIndefinitely),
                _ => {}
            }
        }
    }
    pub fn step(&mut self) -> Result<Status, Fault> {
        trace!("BEGIN macrostep");
        self.status = self.macrostep()?;
        trace!("END macrostep: status={:?}", self.status);
        Ok(self.status.clone())
    }
    fn macrostep(&mut self) -> Result<Status, Fault> {
        match self.status {
            Status::New => {
                let start_t = TransitionBuilder::default()
                    .target_label(Some(self.root.node().label().clone()))
                    .build()
                    .unwrap();
                let start_st = self.root.clone();
                return match self.microstep(start_st, &start_t) {
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
            let st = match self.current_config[i].upgrade() {
                None => return Err(Fault::CurrentStateUndefined),
                ref sst => sst.clone().unwrap(),
            };
            if let State::Final(ref f) = *st {
                for on_exit in f.on_exit() {
                    on_exit.actionable().apply(self)?;
                }
                results.push(Status::Done(f.result.outputable().eval(self)));
                continue;
            }
            match self.eventless_transition(st.clone()) {
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
                let st = match self.current_config[i].upgrade() {
                    None => return Err(Fault::CurrentStateUndefined),
                    ref sst => sst.clone().unwrap(),
                };
                match self.event_transition(st.clone(), ev) {
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
            let st = match self.current_config[i].upgrade() {
                None => return Err(Fault::CurrentStateUndefined),
                ref sst => sst.clone().unwrap(),
            };
            if let Some(label) = st.node().initial() {
                let status = self.microstep(st,
                               &TransitionBuilder::default()
                                   .target_label(Some(label))
                                   .build()
                                   .unwrap())?;
                results.push(status);
            } else if let State::Parallel(ref p) = *st {
                for sub_st in p.substates.borrow().iter() {
                    let status = self.microstep(st.clone(),
                                   &TransitionBuilder::default()
                                       .target_label(Some(sub_st.node().label().to_string()))
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
    fn eventless_transition(&mut self, st: Rc<State>) -> Result<Option<Status>, Fault> {
        let mut sst = Some(st);
        loop {
            sst = match sst {
                Some(st) => {
                    if let Some(n) = st.active_node() {
                        for t in n.transitions() {
                            if t.topics.is_empty() && t.cond.conditional().eval(self) {
                                let status = self.microstep(st.clone(), t)?;
                                return Ok(Some(status));
                            }
                        }
                        match n.parent().upgrade() {
                            None => None,
                            p => p,
                        }
                    } else {
                        None
                    }
                }
                None => return Ok(None),
            };
        }
    }
    fn event_transition(&mut self, st: Rc<State>, ev: &Event) -> Result<Option<Status>, Fault> {
        let mut sst = Some(st);
        loop {
            sst = match sst {
                Some(st) => {
                    if let Some(n) = st.active_node() {
                        for t in n.transitions() {
                            if t.topics.contains(&ev.topic) && t.cond.conditional().eval(self) {
                                let status = self.microstep(st.clone(), t)?;
                                return Ok(Some(status));
                            }
                        }
                        match n.parent().upgrade() {
                            None => None,
                            p => p,
                        }
                    } else {
                        None
                    }
                }
                None => return Ok(None),
            };
        }
    }
    fn microstep(&mut self, current_st: Rc<State>, t: &Transition) -> Result<Status, Fault> {
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
                let target_st = match self.state(target_label) {
                    Some(r) => r,
                    None => return Err(Fault::LabelNotFound(target_label.clone())),
                };
                let current_id = current_st.node().id().clone();
                // Execute on_exit for all states we are leaving in this transition.
                let exit_states = Context::exit_states(&current_id, target_st.node().id());
                for id in exit_states {
                    let exit_state = match self.state_by_id(&id) {
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
                let entry_states = Context::entry_states(&current_id, target_st.node().id());
                for id in entry_states {
                    let entry_state = match self.state_by_id(&id) {
                        Some(st) => st,
                        None => return Err(Fault::IDNotFound(id.clone())),
                    };
                    for on_entry in entry_state.node().on_entry() {
                        on_entry.actionable().apply(self)?;
                    }
                }
                self.next_config.push(Rc::downgrade(&target_st));
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
        let common = Context::common_ancestor(from, to);
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
        let common = Context::common_ancestor(from, to);
        let mut result = vec![];
        for i in common.len()..to.len() {
            let id = to[0..i + 1].to_vec();
            result.push(id);
        }
        result
    }
}

pub trait Actionable {
    fn apply(&self, &mut Context) -> Result<(), Fault>;
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
        Action::Log(LogBuilder::default()$(.$key($value))*.build().unwrap())
    }
}

impl Actionable for Log {
    fn apply(&self, _: &mut Context) -> Result<(), Fault> {
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
        Action::Raise(RaiseBuilder::default()$(.$key($value))*.build().unwrap())
    }
}

impl Actionable for Raise {
    fn apply(&self, ctx: &mut Context) -> Result<(), Fault> {
        trace!("raise {:?}", self);
        ctx.events.push(Event {
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
        Action::Assign(AssignBuilder::default()$(.$key($value))*.build().unwrap())
    }
}

impl Actionable for Assign {
    fn apply(&self, ctx: &mut Context) -> Result<(), Fault> {
        // TODO: partial assignment into objects & lists
        ctx.vars.insert(self.key.to_string(), self.value.clone());
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
    fn apply(&self, ctx: &mut Context) -> Result<(), Fault> {
        for &(ref cond, ref action) in &self.when {
            if cond.conditional().eval(ctx) {
                return action.actionable().apply(ctx);
            }
        }
        if let &Some(ref action) = &self.otherwise {
            return action.actionable().apply(ctx);
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct ActionFn {
    f: Rc<Fn(&mut Context) -> Result<(), Fault>>,
}

#[macro_export]
macro_rules! action_fn {
    ($f:expr) => {
        Action::Fn(ActionFn::new($f))
    }
}

impl std::fmt::Debug for ActionFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "(ActionFn)")
    }
}

impl PartialEq for ActionFn {
    fn eq(&self, _: &Self) -> bool {
        return false;
    }
}

impl Actionable for ActionFn {
    fn apply(&self, ctx: &mut Context) -> Result<(), Fault> {
        (self.f)(ctx)
    }
}

impl ActionFn {
    pub fn new(f: Rc<Fn(&mut Context) -> Result<(), Fault>>) -> ActionFn {
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
    fn eval(&self, &Context) -> bool;
}

#[derive(Debug, PartialEq, Clone)]
pub struct True;

impl Conditional for True {
    fn eval(&self, _: &Context) -> bool {
        true
    }
}

#[derive(Clone)]
pub struct CondFn {
    f: Rc<Fn(&Context) -> bool>,
}

impl std::fmt::Debug for CondFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "(CondFn)")
    }
}

impl PartialEq for CondFn {
    fn eq(&self, _: &Self) -> bool {
        return false;
    }
}

impl Conditional for CondFn {
    fn eval(&self, ctx: &Context) -> bool {
        (self.f)(ctx)
    }
}

impl CondFn {
    pub fn new(f: Rc<Fn(&Context) -> bool>) -> CondFn {
        CondFn { f: f }
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
    ValueOf(ValueOf),
}

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct ValueOf {
    #[builder(setter(into))]
    key: String,
}

impl Outputable for ValueOf {
    fn eval(&self, ctx: &Context) -> Value {
        match ctx.vars.get(&self.key) {
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

#[derive(Debug, PartialEq, Clone, Builder)]
pub struct Transition {
    #[builder(default="HashSet::new()")]
    topics: HashSet<String>,
    #[builder(default="Condition::True(True)")]
    cond: Condition,
    #[builder(default="vec![]")]
    actions: Vec<Action>,
    #[builder(default="None")]
    target_label: Option<StateLabel>,
}

pub mod agent;

#[macro_export]
macro_rules! state {
    ($label:ident {$($tail:tt)*}) => {{
       let mut stb = AtomicBuilder::default();
       stb.label(stringify!($label));
       state_props!(stb {$($tail)*});
       State::Atomic(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! states {
    ($label:ident {$($tail:tt)*}) => {{
        let mut stb = CompoundBuilder::default();
        stb.label(stringify!($label));
        state_props!(stb {$($tail)*});
        State::Compound(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! parallel {
    ($label:ident {$($tail:tt)*}) => {{
        let mut stb = ParallelBuilder::default();
        stb.label(stringify!($label));
        state_props!(stb {$($tail)*});
        State::Parallel(stb.build().unwrap())
    }}
}

#[macro_export]
macro_rules! final_state {
    ($label:ident {$($tail:tt)*}) => {{
        let mut stb = FinalBuilder::default();
        stb.label(stringify!($label));
        state_props!(stb {$($tail)*});
        State::Final(stb.build().unwrap())
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
        $stb.substates(std::cell::RefCell::new(
            $value.iter().map(|x|{std::rc::Rc::new(x.clone())}).collect()));
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
        $t.target_label(Some(stringify!($value).to_string()));
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
        let mut t = TransitionBuilder::default();
        goto!{@prop t, $key, $value};
        t.build().unwrap()
    }};
    ($key:ident: $value:tt, $($tail:tt)*) => {{
        let mut t = TransitionBuilder::default();
        goto!{@prop t, $key, $value};
        goto!{@props t, $($tail)*};
        t.build().unwrap()
    }};
}

#[macro_export]
macro_rules! cond_fn {
    ($f:expr) => {
        Condition::Fn(CondFn::new(Rc::new($f)))
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
        let mut c = ChooseBuilder::default();
        choose!{@prop c, $key, $value};
        Action::Choose(c.build().unwrap())
    }};
    ($key:ident: $value:tt, $($tail:tt)*) => {{
        let mut c = ChooseBuilder::default();
        choose!{@prop c, $key, $value};
        choose!{@props c, $($tail)*};
        Action::Choose(c.build().unwrap())
    }};
}
