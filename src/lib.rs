#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate log;

use std::cmp::{max, Ordering};
use std::collections::{HashMap, HashSet};
use std::iter;
use std::rc::Rc;
use std::slice::{Iter, IterMut};

extern crate chrono;

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
    fn allstates(st: &State) -> Vec<&State> {
        iter::once(st).chain(st.substates().flat_map(|ss| State::allstates(ss))).collect()
    }
    fn substates(&self) -> Iter<State> {
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

pub struct Interpreter {
    vars: Object,
    events: EventQueue,
    status: Status,
    current_event: Option<Event>,
    current_config: Vec<StateID>,
    next_config: Vec<StateID>,
    exiting_parallels: HashSet<StateID>,
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
        }
    }
    pub fn get_var(&self, key: &str) -> Option<&Value> {
        self.vars.get(key)
    }
    pub fn set_var(&mut self, key: &str, value: Value) {
        self.vars.insert(key.to_owned(), value);
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
                    .target_label(Some(ctx.root.node().label().clone()))
                    .build()
                    .unwrap();
                return match self.microstep(ctx, &ctx.root, &start_t) {
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
                results.push(Status::Done(f.result.outputable().eval(self)));
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
            } else if let &State::Parallel(ref p) = st {
                for sub_st in p.substates.iter() {
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
        Action::Log(LogBuilder::default()$(.$key($value))*.build().unwrap())
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
        Action::Raise(RaiseBuilder::default()$(.$key($value))*.build().unwrap())
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
        Action::Assign(AssignBuilder::default()$(.$key($value))*.build().unwrap())
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

#[derive(Clone)]
pub struct ActionFn {
    f: Rc<Fn(&mut Interpreter) -> Result<(), Fault>>,
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
    fn eval(&self, it: &Interpreter) -> bool {
        (self.f)(it)
    }
}

impl CondFn {
    pub fn new(f: Rc<Fn(&Interpreter) -> bool>) -> CondFn {
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
