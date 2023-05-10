# Numl(ML_LIKE_TYPE_SYSTEM)

It is well-known that numerical computations may sometimes lead to wrong results because of the accumulation
of roundoff errors. we propose a ML-like type system (strong, implicit, polymorphic) for
numerical computations in finite precision, in which the type of an arithmetic expression carries information
on its accuracy. We use dependent types and a type inference algorithm which, from the user point of view,
acts like ML type inference algorithm even if it slightly differs in its implementation. While type systems have
been widely used to prevent a large variety of software bugs, to our knowledge, no type system has been
targeted to address numerical accuracy issues in finite precision computations. Basically, our type system
accepts expressions for which it may ensure a certain accuracy on the result of the evaluation and it rejects
expressions for which a minimal accuracy on the result of the evaluation cannot be inferred. The soundness
of the type system is ensured by a subject reduction theorem and we show that our type system does is able
to type usual implementations of usual simple numerical algorithms.
