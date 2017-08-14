#[cfg(test)]

use std::cell::RefCell;
use std::rc::Rc;

extern crate statechart;
use statechart::*;

#[test]
fn hello_world() {
    let st = AtomicBuilder::default()
        .label("S")
        .on_entry(vec![Action::Log(LogBuilder::default().message("hello world").build().unwrap())])
        .build()
        .unwrap();
    println!("{:?}", st);
}

#[test]
fn transitions() {
    let ctx = Context::new(State::Compound(c123()));
    let st = ctx.root();
    assert_eq!(st.node().id(), &vec![0]);
    assert_eq!(st.node().substate("S1").unwrap().node().id(), &vec![0, 0]);
    assert_eq!(st.node().substate("S2").unwrap().node().id(), &vec![0, 1]);
    assert_eq!(st.node().substate("S3").unwrap().node().id(), &vec![0, 2]);
    assert_eq!(st.node().substate("nonesuch"), None);
}

#[test]
fn context() {
    let st = State::Compound(c123());
    let ctx = Context::new(st);
    assert_eq!(ctx.state("S1").unwrap().node().label(), "S1");
    for ss in vec!["S1", "S2", "S3"] {
        assert_eq!(ctx.state(ss).unwrap().node().parent().upgrade().unwrap().node().label(),
                   "S");
    }
    assert_eq!(ctx.state("S").unwrap().node().parent().upgrade(), None);
}

fn c123() -> Compound {
    CompoundBuilder::default()
        .label("S")
        .substates(RefCell::new(vec![Rc::new(State::Atomic(AtomicBuilder::default()
                                         .label("S1")
                                         .transitions(vec![TransitionBuilder::default()
                                                  .target_label(Some("S2".to_string()))
                                                  .build()
                                                  .unwrap()])
                                         .on_entry(vec![Action::Log(LogBuilder::default()
                                                            .message("hello s1")
                                                            .build()
                                                            .unwrap())])
                                         .build()
                                         .unwrap())),
                                     Rc::new(State::Atomic(AtomicBuilder::default()
                                         .label("S2")
                                         .transitions(vec![TransitionBuilder::default()
                                                  .target_label(Some("S3".to_string()))
                                                  .build()
                                                  .unwrap()])
                                         .on_entry(vec![Action::Log(LogBuilder::default()
                                                            .message("hello s2")
                                                            .build()
                                                            .unwrap())])
                                         .build()
                                         .unwrap())),
                                     Rc::new(State::Final(FinalBuilder::default()
                                         .label("S3")
                                         .on_entry(vec![Action::Log(LogBuilder::default()
                                                            .message("hello s3")
                                                            .build()
                                                            .unwrap())])
                                         .on_exit(vec![Action::Log(LogBuilder::default()
                                                           .message("and goodbye now")
                                                           .build()
                                                           .unwrap())])
                                         .build()
                                         .unwrap()))]))
        .build()
        .unwrap()
}
