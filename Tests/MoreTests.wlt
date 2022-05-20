VerificationTest[
    1+1,
    2,
    TestID -> "OnePlusOne"
]

VerificationTest[
    1+1,
    3,
    TestID -> "OnePlusOneFailed"
]

VerificationTest[
    1/0,
    ComplexInfinity,
    TestID -> "MessageFailure"
]

VerificationTest[
    Pause[5]; 1+1,
    2,
    TestID -> "SlowEvaluation",
    TimeConstraint -> 1
]

VerificationTest[
    Range[10000]; 1+1,
    2,
    TestID -> "BigEvaluation",
    MemoryConstraint -> 1000
]

VerificationTest[
    Pause[5]; 1+1,
    2,
    TestID -> "Timeout",
    TimeConstraint -> 1
]