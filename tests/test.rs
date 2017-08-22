#[cfg(test)]

use std::collections::HashMap;

extern crate statechart;
use statechart::*;

#[test]
fn hello_world() {
    let st = state!("S", on_entry: vec![action_log!(message: "hello world")]);
    println!("{:?}", st);
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

fn c123() -> State {
    states!("S", substates: substates!(
        state!("S1",
            transitions: vec![goto!(target_label: Some("S2".to_string()))],
            on_entry: vec![action_log!(message: "hello s1")]),
        state!("S2",
            transitions: vec![goto!(target_label: Some("S3".to_string()))],
            on_entry: vec![action_log!(message: "hello s2")]),
        final_state!("S3",
            on_entry: vec![action_log!(message: "hello s3")],
            on_exit: vec![action_log!(message: "and goodbye now")])))
}
