# Monoscope Email Onboarding Sequence

---

## [···] Welcome Email

**Subject:** [···] Welcome to Monoscope

![Monoscope Logo](monoscope.png)

---

Hi {{first_name}},

Welcome to Monoscope! We're glad you're here.

Monoscope is a unified observability platform for logs, traces, metrics, and APIs. Your data is stored in your own S3 bucket with unlimited retention, no surprise bills, and AI-powered search that actually understands what you're looking for.

### Next steps to get started

- **[Connect your S3 bucket](https://monoscope.tech/docs/getting-started/s3-setup)**: Own your data, pay nothing for retention
- **[Send your first telemetry](https://monoscope.tech/docs/getting-started/connect)**: OpenTelemetry setup in under 5 minutes
- **[Explore with AI](https://monoscope.tech/docs/getting-started/ai-queries)**: Query logs, traces, and metrics in plain English

---

[**Explore the Docs**](https://monoscope.tech/docs)

---

If you have any questions or need some help, reply directly to this email.

— The Monoscope Team

---

---

## [···] Your data, your bucket (1/6)

**Subject:** [···] Stop paying for log retention (1/6)

![Monoscope Logo](monoscope.png)

[Open Monoscope](https://app.monoscope.tech)

---

Hi {{first_name}},

Over the next week, we're showcasing a feature a day to help you get the most out of Monoscope.

---

First up: **bring your own S3 bucket** and say goodbye to retention fees.

![S3 Bucket Architecture Demo](#)

Other observability tools charge per GB ingested _and_ retained. With Monoscope, all your telemetry (logs, traces, metrics, everything) lives in your own S3 bucket. You control the data and the costs.

💰 Zero retention fees. Keep years of logs, traces, and metrics

🔒 Your data never leaves your infrastructure

📊 Query historical data without paying extra

---

No more choosing between visibility and budget.

[**Connect your bucket**](https://monoscope.tech/docs/s3-setup)

---

If you have any questions or need some help, reply directly to this email.

— The Monoscope Team

---

---

## [···] Logs and traces, finally together (2/6)

**Subject:** [···] Logs and traces, finally in the same view (2/6)

![Monoscope Logo](monoscope.png)

[Open Monoscope](https://app.monoscope.tech)

---

Hi {{first_name}},

In most tools, logs and traces live in separate tabs. In Monoscope, **they're merged into a single tree view.**

---

See exactly which service logged what, while processing which customer request. All in context.

![Unified Tree View Demo](#)

🌳 Logs appear inline with their parent spans

🔗 Trace a request across services with full log context

🎯 No more jumping between tabs to correlate data

---

When something breaks at 3am, you'll know exactly where and why. No need to open five different tools.

[**See it in action**](https://app.monoscope.tech)

---

If you have any questions or need some help, reply directly to this email.

— The Monoscope Team

---

---

## [···] Ask your data anything (3/6)

**Subject:** [···] Show me errors from the last hour (3/6)

![Monoscope Logo](monoscope.png)

[Open Monoscope](https://app.monoscope.tech)

---

Hi {{first_name}},

Forget query languages. With Monoscope, **just type what you're looking for in plain English.**

---

Our AI-powered explorer turns natural language into queries and charts instantly. It works across logs, traces, metrics, and more.

![AI Query Demo](#)

💬 "Show me error rates by service over the last hour"

📈 "Plot p99 latency for the checkout endpoint this week"

🔎 "Find all failed database queries from the payments service"

🗄️ "Show me slow queries taking longer than 500ms"

---

Type a question. Get a chart, filtered logs, or trace details. No PromQL. No LogQL. No learning curve.

[**Try AI queries**](https://app.monoscope.tech/explore)

---

If you have any questions or need some help, reply directly to this email.

— The Monoscope Team

---

---

## [···] Catch breaking API changes instantly (4/6)

**Subject:** [···] Catch breaking API changes before your users do (4/6)

![Monoscope Logo](monoscope.png)

[Open Monoscope](https://app.monoscope.tech)

---

Hi {{first_name}},

Beyond logs and metrics, Monoscope has a unique capability: **automatic API change detection.**

---

We fingerprint your API responses and detect schema changes before your customers report bugs.

![API Change Detection Demo](#)

🔍 Detect new, removed, or changed fields automatically

🚨 Get alerted on breaking changes in real-time

📜 See exactly what changed, when, and in which endpoint

---

No more "the API changed and nobody told us" incidents. This works alongside your logs, traces, and metrics to give you complete visibility.

[**Learn how it works**](https://monoscope.tech/docs/api-changes)

---

If you have any questions or need some help, reply directly to this email.

— The Monoscope Team

---

---

## [···] Anomaly detection that learns (5/6)

**Subject:** [···] Alerts that learn what's normal (5/6)

![Monoscope Logo](monoscope.png)

[Open Monoscope](https://app.monoscope.tech)

---

Hi {{first_name}},

Monoscope doesn't just store your telemetry. It **watches for anomalies** across your entire stack.

---

Set up intelligent alerts on logs, metrics, traces, and API behavior. They adapt to your traffic patterns and catch issues before they escalate.

![Anomaly Detection Demo](#)

📈 Automatic baseline learning across all your telemetry

🎯 Reduce alert fatigue with smart thresholds

🐢 Catch slow database queries, error spikes, latency regressions

🔔 Slack, Discord, email, webhook. Your choice

---

Get notified about what matters, not what's normal.

[**Create your first alert**](https://app.monoscope.tech/)

---

If you have any questions or need some help, reply directly to this email.

— The Monoscope Team

---

---

## [···] Build dashboards with plain English (6/6)

**Subject:** [···] Build dashboards with plain English (6/6)

![Monoscope Logo](monoscope.png)

[Open Monoscope](https://app.monoscope.tech)

---

Hi {{first_name}},

Finally, turn all your telemetry into **dashboards** that give your team instant visibility.

---

Track service health, error rates, database performance, API latency. All powered by the same AI queries.

![Dashboard Builder Demo](#)

📊 Visualize logs, metrics, traces, and API data in one place

💬 Create charts with natural language

👥 Share with your team or embed anywhere

---

That wraps up our feature tour! You now have the tools to:

- Store unlimited logs, traces, and metrics in your own S3
- Debug with logs and traces in context
- Query everything with plain English
- Catch API schema changes automatically
- Get alerted on anomalies across your stack
- Build dashboards your team will actually use

Reply to this email if you'd like a personalized walkthrough.

— The Monoscope Team

---

---

# Email Timing

| Email                       | Trigger                         |
| --------------------------- | ------------------------------- |
| Welcome                     | Immediately on signup           |
| 1/6 - S3 / BYOB             | Day 1 (few hours after welcome) |
| 2/6 - Unified Logs + Traces | Day 2                           |
| 3/6 - AI Query Explorer     | Day 4                           |
| 4/6 - API Change Detection  | Day 6                           |
| 5/6 - Anomaly Detection     | Day 8                           |
| 6/6 - Dashboards            | Day 10                          |

---

# Subject Lines Summary

| Email   | Subject                                                     |
| ------- | ----------------------------------------------------------- |
| Welcome | [···] Welcome to Monoscope                                  |
| 1/6     | [···] Stop paying for log retention (1/6)                   |
| 2/6     | [···] Logs and traces, finally in the same view (2/6)       |
| 3/6     | [···] "Show me errors from the last hour" (3/6)             |
| 4/6     | [···] Catch breaking API changes before your users do (4/6) |
| 5/6     | [···] Alerts that learn what's normal (5/6)                 |
| 6/6     | [···] Build dashboards with plain English (6/6)             |

---

# Key Differentiators by Email

| Email | Differentiator       | Why it matters                                                                              |
| ----- | -------------------- | ------------------------------------------------------------------------------------------- |
| 1/6   | BYOB S3 storage      | Cost is a massive pain point with Datadog/Coralogix. Zero retention fees for all telemetry. |
| 2/6   | Unified tree view    | Logs in context with traces. Most tools separate these into tabs.                           |
| 3/6   | AI natural language  | Query logs, traces, metrics, DB queries in plain English. No PromQL/LogQL.                  |
| 4/6   | API change detection | Unique to Monoscope. Catches schema breaks before users report them.                        |
| 5/6   | Smart anomaly alerts | Baseline learning across entire stack, not just static thresholds.                          |
| 6/6   | Unified dashboards   | Logs, metrics, traces, APIs in one dashboard with AI-powered charting.                      |

---

# Implementation Notes

- Replace `#` image placeholders with product screenshots/GIFs
- Consider A/B testing subject lines:
  - "Stop paying for log retention" vs "Your logs, your bucket, zero fees"
  - "Welcome to Monoscope" vs "Welcome to Monoscope 👋"
- Skip emails for features users have already activated (conditional sends)
- Track which email drives the most activations to inform future messaging
- OpenTelemetry compatibility could be mentioned in welcome email since it's table stakes for adoption
