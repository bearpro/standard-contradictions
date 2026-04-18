import mdl


def test_typed_integer_relations_detect_inconsistency() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.age = mdl.Var(mdl.INT)
        doc.adult = mdl.Rule("O", None, doc.age >= 18)
        doc.minor = mdl.Rule("O", None, doc.age < 18)

    assert not mdl.solve(doc.build()).is_consistent


def test_typed_arithmetic_and_string_length_are_solved() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.count = mdl.Var(mdl.INT)
        doc.name = mdl.Var(mdl.STRING)
        doc.amount = mdl.Rule("O", None, doc.count + 2 >= 4)
        doc.name_length = mdl.Rule("O", None, mdl.Len(doc.name) > 2)

    result = mdl.solve(doc.build())

    assert result.is_consistent


def test_if_formula_selects_branch() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.active = mdl.Var(mdl.BOOL)
        doc.age = mdl.Var(mdl.INT)
        doc.r1 = mdl.Rule("O", None, mdl.Truth(doc.active))
        doc.r2 = mdl.Rule(
            "O",
            None,
            mdl.If(mdl.Truth(doc.active), doc.age >= 18, doc.age >= 0),
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent


def test_product_fields_are_sugar_over_typed_terms() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.person_type = mdl.ProductType({"age": mdl.INT, "name": mdl.STRING})
        doc.person = mdl.Var(doc.person_type)
        doc.age_rule = mdl.Rule("O", None, doc.person.field("age") >= 18)
        doc.name_rule = mdl.Rule(
            "O",
            None,
            mdl.Len(doc.person.field("name")) > 0,
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent


def test_two_single_string_field_products_can_have_different_constraints() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.left_type = mdl.ProductType({"value": mdl.STRING})
        doc.right_type = mdl.ProductType({"value": mdl.STRING})
        doc.left = mdl.Var(doc.left_type)
        doc.right = mdl.Var(doc.right_type)
        doc.left_value = mdl.Rule(
            "O",
            None,
            mdl.Eq(doc.left.field("value"), mdl.String("test")),
        )
        doc.right_value_length = mdl.Rule(
            "O",
            None,
            mdl.Len(doc.right.field("value")) > 4,
        )

    module = doc.build()
    result = mdl.solve(module)

    assert result.is_consistent
    assert result.trace is not None
    left_value = module.left_value
    right_value_length = module.right_value_length
    assert isinstance(left_value, mdl.Rule)
    assert isinstance(right_value_length, mdl.Rule)
    assert mdl.evaluate(left_value.consequent, result.trace)
    assert mdl.evaluate(right_value_length.consequent, result.trace)


def test_sum_case_selects_variant_branch() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.person_type = mdl.ProductType({"age": mdl.INT, "name": mdl.STRING})
        doc.decision_type = mdl.SumType(
            {"Approved": doc.person_type, "Rejected": mdl.STRING},
        )
        doc.decision = mdl.Var(doc.decision_type)
        doc.approved = mdl.VariantPayload(doc.decision, "Approved")
        doc.rejected = mdl.VariantPayload(doc.decision, "Rejected")
        doc.must_be_approved = mdl.Rule(
            "O",
            None,
            mdl.IsVariant(doc.decision, "Approved"),
        )
        doc.valid_payload = mdl.Rule(
            "O",
            None,
            mdl.Case(
                doc.decision,
                {
                    "Approved": doc.approved.field("age") >= 18,
                    "Rejected": mdl.Len(doc.rejected) > 0,
                },
            ),
        )

    result = mdl.solve(doc.build())

    assert result.is_consistent


def test_exact_name_alignment_matches_shared_builder_names() -> None:
    with mdl.ModuleBuilder() as left:
        left.status = mdl.Proposition()
        left.age = mdl.Var(mdl.INT)

    with mdl.ModuleBuilder() as right:
        right.status = mdl.Proposition()
        right.name = mdl.Var(mdl.STRING)

    matches = mdl.align(left.build(), right.build())

    assert matches == (mdl.Alignment("status", "status", 1.0),)
