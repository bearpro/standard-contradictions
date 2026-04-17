import mdl


def test_typed_integer_relations_detect_inconsistency() -> None:
    with mdl.ModuleBuilder() as doc:
        age = mdl.Var("age", mdl.INT)
        doc.age = age
        doc.adult = mdl.Rule("adult", "O", None, age >= 18)
        doc.minor = mdl.Rule("minor", "O", None, age < 18)

    assert not mdl.solve(doc.build()).is_consistent


def test_typed_arithmetic_and_string_length_are_solved() -> None:
    with mdl.ModuleBuilder() as doc:
        count = mdl.Var("count", mdl.INT)
        name = mdl.Var("name", mdl.STRING)
        doc.count = count
        doc.name = name
        doc.amount = mdl.Rule("amount", "O", None, count + 2 >= 4)
        doc.name_length = mdl.Rule("name-length", "O", None, mdl.Len(name) > 2)

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert result.trace is not None
    state = result.trace[0]
    count = state.values["count"]
    name = state.values["name"]
    assert isinstance(count, int)
    assert isinstance(name, str)
    assert count >= 2
    assert len(name) > 2


def test_if_formula_selects_branch() -> None:
    with mdl.ModuleBuilder() as doc:
        active = mdl.Var("active", mdl.BOOL)
        age = mdl.Var("age", mdl.INT)
        doc.active = active
        doc.age = age
        doc.r1 = mdl.Rule("active", "O", None, mdl.Truth(active))
        doc.r2 = mdl.Rule(
            "conditional-age",
            "O",
            None,
            mdl.If(mdl.Truth(active), age >= 18, age >= 0),
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert result.trace is not None
    assert result.trace[0].values["active"] is True
    age = result.trace[0].values["age"]
    assert isinstance(age, int)
    assert age >= 18


def test_product_fields_are_sugar_over_typed_terms() -> None:
    person_type = mdl.ProductType("Person", {"age": mdl.INT, "name": mdl.STRING})

    with mdl.ModuleBuilder() as doc:
        person = mdl.Var("person", person_type)
        doc.person_type = person_type
        doc.person = person
        doc.age_rule = mdl.Rule("age", "O", None, person.field("age") >= 18)
        doc.name_rule = mdl.Rule(
            "name",
            "O",
            None,
            mdl.Len(person.field("name")) > 0,
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert result.trace is not None
    person = result.trace[0].values["person"]
    assert isinstance(person, mdl.ProductInstance)
    age = person.field_value("age")
    name = person.field_value("name")
    assert isinstance(age, int)
    assert isinstance(name, str)
    assert age >= 18
    assert len(name) > 0


def test_two_single_string_field_products_can_have_different_constraints() -> None:
    left_type = mdl.ProductType("LeftBox", {"value": mdl.STRING})
    right_type = mdl.ProductType("RightBox", {"value": mdl.STRING})

    with mdl.ModuleBuilder() as doc:
        left = mdl.Var("left", left_type)
        right = mdl.Var("right", right_type)
        doc.left_type = left_type
        doc.right_type = right_type
        doc.left = left
        doc.right = right
        doc.left_value = mdl.Rule(
            "left-value",
            "O",
            None,
            mdl.Eq(left.field("value"), mdl.String("test")),
        )
        doc.right_value_length = mdl.Rule(
            "right-value-length",
            "O",
            None,
            mdl.Len(right.field("value")) > 4,
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert result.trace is not None
    left = result.trace[0].values["left"]
    right = result.trace[0].values["right"]
    assert isinstance(left, mdl.ProductInstance)
    assert isinstance(right, mdl.ProductInstance)
    assert left.field_value("value") == "test"
    right_value = right.field_value("value")
    assert isinstance(right_value, str)
    assert len(right_value) > 4


def test_sum_case_selects_variant_branch() -> None:
    person_type = mdl.ProductType("Person", {"age": mdl.INT, "name": mdl.STRING})
    decision_type = mdl.SumType(
        "Decision",
        {"Approved": person_type, "Rejected": mdl.STRING},
    )

    with mdl.ModuleBuilder() as doc:
        decision = mdl.Var("decision", decision_type)
        approved = mdl.VariantPayload(decision, "Approved")
        rejected = mdl.VariantPayload(decision, "Rejected")
        doc.person_type = person_type
        doc.decision_type = decision_type
        doc.decision = decision
        doc.must_be_approved = mdl.Rule(
            "approved",
            "O",
            None,
            mdl.IsVariant(decision, "Approved"),
        )
        doc.valid_payload = mdl.Rule(
            "payload",
            "O",
            None,
            mdl.Case(
                decision,
                {
                    "Approved": approved.field("age") >= 18,
                    "Rejected": mdl.Len(rejected) > 0,
                },
            ),
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent
    assert result.trace is not None
    decision = result.trace[0].values["decision"]
    assert isinstance(decision, mdl.VariantInstance)
    assert decision.variant == "Approved"
    assert isinstance(decision.payload, mdl.ProductInstance)
    age = decision.payload.field_value("age")
    assert isinstance(age, int)
    assert age >= 18


def test_exact_name_alignment_matches_shared_builder_names() -> None:
    with mdl.ModuleBuilder() as left:
        left.status = mdl.Proposition("status")
        left.age = mdl.Var("age", mdl.INT)

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition("status")
        right.name = mdl.Var("name", mdl.STRING)

    matches = mdl.align(left.build(), right.build())

    assert matches == (mdl.Alignment("status", "status", 1.0),)
