import mdl


def test_single_obligation_without_temporality_is_consistent() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.r1 = mdl.Rule("r1", "O", None, doc.p)

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert [rule.source for rule in result.winning_rules] == ["r1"]


def test_opposite_obligations_without_temporality_are_inconsistent() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.r1 = mdl.Rule("r1", "O", None, doc.p)
        doc.r2 = mdl.Rule("r2", "O", None, mdl.Not(doc.p))

    result = mdl.solve(doc.build())

    assert not result.is_consistent
    assert result.trace is None


def test_same_named_obligations_in_different_documents_without_alignment_are_consistent() -> None:
    with mdl.ModuleBuilder() as left:
        left.status = mdl.Proposition("status")
        left.require_status = mdl.Rule("left-requires-status", "O", None, left.status)

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition("status")
        right.forbid_status = mdl.Rule("right-forbids-status", "F", None, right.status)

    result = mdl.solve(left.build(), right.build())

    assert result.is_consistent


def test_trivial_alignment_detects_conflicting_obligations_between_documents() -> None:
    with mdl.ModuleBuilder() as left:
        left.status = mdl.Proposition("status")
        left.require_status = mdl.Rule("left-requires-status", "O", None, left.status)

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition("status")
        right.forbid_status = mdl.Rule("right-forbids-status", "F", None, right.status)

    left_doc = left.build()
    right_doc = right.build()
    alignments = mdl.align(left_doc, right_doc)

    assert alignments == (mdl.Alignment("status", "status", 1.0),)

    result = mdl.solve(left_doc, right_doc, alignments=alignments)

    assert not result.is_consistent


def test_forbidden_is_obligation_to_negation() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.r1 = mdl.Rule("r1", "O", None, doc.p)
        doc.r2 = mdl.Rule("r2", "F", None, doc.p)

    assert not mdl.solve(doc.build()).is_consistent


def test_defeasible_priority_resolves_conflicting_obligations() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.general = mdl.DefeasibleRule("general", "O", None, doc.p)
        doc.exception = mdl.DefeasibleRule("exception", "F", None, doc.p)
        doc.priority = mdl.Priority(doc.exception, doc.general)

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert [rule.source for rule in result.winning_rules] == ["exception"]


def test_defeater_blocks_without_adding_requirement() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.general = mdl.DefeasibleRule("general", "O", None, doc.p)
        doc.blocker = mdl.Defeater("blocker", "O", None)
        doc.priority = mdl.Priority(doc.blocker, doc.general)

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert [rule.source for rule in result.winning_rules] == ["blocker"]


def test_strict_rule_is_not_defeated_by_priority() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.strict = mdl.StrictRule("strict", "O", None, doc.p)
        doc.exception = mdl.DefeasibleRule("exception", "F", None, doc.p)
        doc.priority = mdl.Priority(doc.exception, doc.strict)

    assert not mdl.solve(doc.build()).is_consistent


def test_next_obligation_uses_finite_trace_semantics() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.p = mdl.Proposition("p")
        doc.r1 = mdl.Rule("r1", "O", None, mdl.Next(doc.p))

    document = doc.build()

    assert not mdl.solve(document, horizon=1).is_consistent

    result = mdl.solve(document, horizon=2)

    assert result.is_consistent


def test_example_until_obligations_conflict_on_single_step_trace() -> None:
    with mdl.ModuleBuilder() as doc:
        now = mdl.Proposition("", "now")
        is_x = mdl.Proposition("x")
        is_not_x = mdl.Not(mdl.Proposition("x"))
        now_x = mdl.Until(now, is_x)
        now_not_x = mdl.Until(now, is_not_x)
        doc.now = now
        doc.is_x = is_x
        doc.is_not_x = is_not_x
        doc.now_x = now_x
        doc.now_not_x = now_not_x
        doc.r1 = mdl.Rule("test1", "O", None, now_x)
        doc.r2 = mdl.Rule("test2", "O", None, now_not_x)

    result = mdl.solve(doc.build(), horizon=1)

    assert not result.is_consistent
