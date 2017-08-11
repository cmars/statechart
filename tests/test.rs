#[cfg(test)]

extern crate statechart;
use statechart::*;

#[test]
fn hello_world() {
    let st = Atomic::new("S").push_on_entry(Box::new(Log("hello world".to_string())));
    println!("{:?}", st);
}

#[test]
fn transitions() {
    let st = Compound::new("S")
        .push_substate(Box::new(Atomic::new("S1")
            .push_transition(Transition::new().with_target("S2"))
            .push_on_entry(Box::new(Log("hello s1".to_string())))))
        .push_substate(Box::new(Atomic::new("S2")
            .push_transition(Transition::new().with_target("S3"))
            .push_on_entry(Box::new(Log("hello s2".to_string())))))
        .push_substate(Box::new(Final::new("S3")
            .push_on_entry(Box::new(Log("hello s3".to_string())))));
    println!("{:?}", st);
}
