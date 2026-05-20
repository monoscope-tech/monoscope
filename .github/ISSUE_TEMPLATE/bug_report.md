name: Bug Report
description: Report something that is not working correctly
title: "bug: "
labels: ["bug"]
body:
  - type: markdown
    attributes:
      value: |
        Thank you for reporting a bug! Please fill out all applicable sections to help us reproduce and fix the issue quickly.

  - type: textarea
    id: description
    attributes:
      label: Description
      description: A clear description of the bug. What did you expect to happen, and what actually happened?
      placeholder: |
        Short summary of the bug.
        What were you trying to do?
        What happened instead?
    validations:
      required: true

  - type: textarea
    id: reproduction
    attributes:
      label: Steps to Reproduce
      description: Exact steps to reproduce the bug, in order.
      placeholder: |
        1. Go to '...'
        2. Click on '...'
        3. Scroll down to '...'
        4. See error
    validations:
      required: true

  - type: input
    id: environment
    attributes:
      label: Environment
      description: |
        Include your setup: OS, Docker version, Monoscope version (or commit hash),
        and how you are running Monoscope (Docker Compose, binary, etc.).
      placeholder: "e.g., macOS 14, Docker 26, Monoscope v1.2.3 via docker-compose"

  - type: textarea
    id: logs
    attributes:
      label: Relevant Logs
      description: |
        Paste any relevant error messages, stack traces, or log output.
        If running in Docker, include output from `docker-compose logs monoscope`.
      render: shell

  - type: textarea
    id: context
    attributes:
      label: Additional Context
      description: Anything else that might be relevant (screenshots, workaround, frequency of occurrence)
      render: shell

  - type: checkboxes
    id: requirements
    attributes:
      label: Checklist
      options:
        - label: I have searched for existing issues to confirm this is not a known behavior
          required: true
        - label: I am using the latest stable version of Monoscope
          required: true