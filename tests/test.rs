#[cfg(test)]

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
    let c = CompoundBuilder::default()
        .label("S")
        .substates(vec![State::Atomic(AtomicBuilder::default()
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
                            .unwrap()),
                        State::Atomic(AtomicBuilder::default()
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
                            .unwrap()),
                        State::Final(FinalBuilder::default()
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
                            .unwrap())])
        .build()
        .unwrap();
    println!("{:?}", c);
    let mut st = State::Compound(c);
    st.set_root();
    assert_eq!(st.substate("S1").unwrap().id(), &vec![0, 0]);
    assert_eq!(st.substate("S2").unwrap().id(), &vec![0, 1]);
    assert_eq!(st.substate("S3").unwrap().id(), &vec![0, 2]);
    assert_eq!(st.substate("nonesuch"), None);
}
