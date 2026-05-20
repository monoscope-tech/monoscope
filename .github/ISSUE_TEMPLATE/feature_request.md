name: Feature Request
description: Suggest a new feature or improvement
title: "feat: "
labels: ["enhancement"]
body:
  - type: markdown
    attributes:
      value: |
        Have an idea for a new feature or improvement? We'd love to hear it!

        Please fill out all applicable sections. Well-structured requests with clear use cases get prioritized.

  - type: textarea
    id: problem
    attributes:
      label: Problem or Pain Point
      description: What problem does this solve? Why is this important?
      placeholder: |
        Describe the problem you are facing or the gap you have identified.
        Who is affected by this?
    validations:
      required: true

  - type: textarea
    id: solution
    attributes:
      label: Proposed Solution
      description: Describe your proposed solution in detail.
      placeholder: |
        How would you like this to work? Be as specific as possible.
        Include any API changes, UI changes, or behavioral changes.
    validations:
      required: true

  - type: textarea
    id: alternatives
    attributes:
      label: Alternatives Considered
      description: What other approaches have you considered?
      placeholder: |
        List any alternative solutions or workarounds you considered
        and why you chose your proposed approach.

  - type: textarea
    id: context
    attributes:
      label: Additional Context
      description: |
        Any other context: mockups, diagrams, links to similar features in other tools,
        or anything else that would help us understand the request.
      render: shell

  - type: checkboxes
    id: requirements
    attributes:
      label: Checklist
      options:
        - label: I have searched existing issues and PRs to confirm this is not already being worked on
          required: true
        - label: This feature request is aligned with Monoscope's observability focus
          required: true