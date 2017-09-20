#[cfg(test)]

extern crate env_logger;

#[macro_use]
extern crate statechart;

use std::collections::HashMap;

use statechart::ast::*;
use statechart::interpreter::*;

#[test]
fn blocked_indefinitely() {
    let st = state!{ S {} };
    let ctx = Context::new(st);
    let mut it = Interpreter::new();
    let result = it.run(&ctx);
    assert_eq!(result, Err(Fault::BlockedIndefinitely));
}

#[test]
fn transitions() {
    let ctx = Context::new(c123());
    let st = ctx.root();
    assert_eq!(st.node().id(), &vec![]);
    assert_eq!(st.node().substate("S1").unwrap().node().id(), &vec![0]);
    assert_eq!(st.node().substate("S2").unwrap().node().id(), &vec![1]);
    assert_eq!(st.node().substate("S3").unwrap().node().id(), &vec![2]);
    assert_eq!(st.node().substate("nonesuch"), None);
    assert_eq!(ctx.state_by_id(&vec![0]).unwrap().node().id(), &vec![0]);
    assert_eq!(ctx.state_by_id(&vec![1]).unwrap().node().id(), &vec![1]);
    assert_eq!(ctx.state_by_id(&vec![2]).unwrap().node().id(), &vec![2]);
}

#[test]
fn goto() {
    let g = goto!(target: S1, topics: ["foo", "bar", "baz"]);
    assert_eq!(g,
               TransitionBuilder::default()
                   .target_label(Some("S1".to_string()))
                   .topics(["foo", "bar", "baz"].iter().map(|x| x.to_string()).collect())
                   .build()
                   .unwrap());
}

#[test]
fn context() {
    let ctx = Context::new(c123());
    assert_eq!(ctx.state("S1").unwrap().node().label(), "S1");
    for ss in vec!["S1", "S2", "S3"] {
        assert_eq!(ctx.state_by_id(ctx.state(ss).unwrap().node().parent().unwrap()).unwrap().node().label(),
                   "S");
    }
    assert_eq!(ctx.state("S").unwrap().node().parent(), None);
    let mut it = Interpreter::new();
    let result = it.run(&ctx);
    assert!(result.is_ok(), "{:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Object(HashMap::new()));
}

#[test]
fn parallel() {
    let ctx = Context::new(phello());
    let mut it = Interpreter::new();
    let result = it.run(&ctx);
    assert!(result.is_ok(), "{:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Object(HashMap::new()));
}

#[test]
fn parallel_final() {
    let ctx = Context::new(phellofinal());
    let mut it = Interpreter::new();
    let result = it.run(&ctx);
    assert!(result.is_ok(), "{:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Object(HashMap::new()));
}

#[test]
fn parallel_swap() {
    let ctx = Context::new(pswap());
    let mut it = Interpreter::new();
    for _ in 0..100 {
        let result = it.step(&ctx);
        assert_eq!(result, Ok(Status::Runnable));
    }
}

fn c123() -> State {
    let _ = env_logger::init();
    states!{ S {
        substates: [
            state!{ S1 {
                transitions: [goto!(target: S2)],
                on_entry: [action_log!(message: "hello s1")],
            }},
            state!{ S2 {
                transitions: [goto!(target: S3)],
                on_entry: [action_log!(message: "hello s2")],
            }},
            final_state!{ S3 {
                on_entry: [action_log!(message: "hello s3")],
                on_exit: [action_log!(message: "and goodbye now")],
            }},
        ]}}
}

fn phello() -> State {
    let _ = env_logger::init();
    states!{ S {
        substates: [
            parallel!{ P  {
                substates: [
                    state!{ S1 {
                        transitions: [goto!(target: SF)],
                        on_entry: [action_log!(message: "hello s1")],
                    }},
                    state!{ S2 {
                        on_entry: [action_log!(message: "hello s2")],
                    }},
                    state!{ S3 {
                        on_entry: [action_log!(message: "hello s3")],
                    }},
                ]}},
            final_state!{ SF {
                on_exit: [action_log!(message: "goodbye now")],
            }},
        ]}}
}

fn phellofinal() -> State {
    let _ = env_logger::init();
    states!{ S {
        substates: [
            parallel!{ P  {
                substates: [
                    state!{ S1 {
                        on_entry: [action_log!(message: "hello s1")],
                    }},
                    state!{ S2 {
                        on_entry: [action_log!(message: "hello s2")],
                    }},
                    state!{ S3 {
                        on_entry: [action_log!(message: "hello s3")],
                    }},
                    final_state!{ SF {
                        on_entry: [action_log!(message: "hello sf")],
                        on_exit: [action_log!(message: "goodbye now")],
                    }},
            ]}},
        ]}}
}

fn pswap() -> State {
    let _ = env_logger::init();
    states!{ S {
        substates: [
            parallel!{ P {
                substates: [
                    state!{ S1 {
                        on_entry: [action_log!(message: "s1 wants to be an s2")],
                        transitions: [goto!(target: S2)],
                    }},
                    state!{ S2 {
                        on_entry: [action_log!(message: "s2 wants to be an s1")],
                        transitions: [goto!(target: S1)],
                    }},
                ]}},
            ]}}
}
