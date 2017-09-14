#[cfg(test)]

use std::collections::HashMap;

extern crate env_logger;
extern crate statechart;
use statechart::*;

#[test]
fn blocked_indefinitely() {
    let st = state!{ S {} };
    let mut ctx = Context::new(st);
    let result = ctx.run();
    assert_eq!(result, Err(Fault::BlockedIndefinitely));
}

#[test]
fn transitions() {
    let ctx = Context::new(c123());
    let st = ctx.root();
    assert_eq!(st.node().id(), &vec![0]);
    assert_eq!(st.node().substate("S1").unwrap().node().id(), &vec![0, 0]);
    assert_eq!(st.node().substate("S2").unwrap().node().id(), &vec![0, 1]);
    assert_eq!(st.node().substate("S3").unwrap().node().id(), &vec![0, 2]);
    assert_eq!(st.node().substate("nonesuch"), None);
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
    let mut ctx = Context::new(c123());
    assert_eq!(ctx.state("S1").unwrap().node().label(), "S1");
    for ss in vec!["S1", "S2", "S3"] {
        assert_eq!(ctx.state(ss).unwrap().node().parent().upgrade().unwrap().node().label(),
                   "S");
    }
    assert_eq!(ctx.state("S").unwrap().node().parent().upgrade(), None);
    let result = ctx.run();
    assert!(result.is_ok(), "{:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Object(HashMap::new()));
}

#[test]
fn parallel() {
    let mut ctx = Context::new(phello());
    let result = ctx.run();
    assert!(result.is_ok(), "{:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Object(HashMap::new()));
}

#[test]
fn parallel_final() {
    let mut ctx = Context::new(phellofinal());
    let result = ctx.run();
    assert!(result.is_ok(), "{:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::Object(HashMap::new()));
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
