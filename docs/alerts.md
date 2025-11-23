# Alert Configuration Guide

Configure intelligent alerts in Monoscope using AI-powered anomaly detection.

## AI-Powered Anomaly Detection

Monoscope automatically detects anomalies in your system:

- **Pattern Learning**: AI learns your system's normal behavior patterns
- **Automatic Detection**: No manual threshold configuration required
- **Context Awareness**: Understands time-based and seasonal patterns
- **Intelligent Grouping**: Groups related issues to reduce alert noise

## Notification Channels

### Email Notifications

Configure email alerts using either SendGrid or SMTP:

```env
# Option 1: SendGrid
SENDGRIDAPIKEY=your-sendgrid-key

# Option 2: SMTP
SMTP_HOST=smtp.example.com
SMTP_PORT=587
SMTP_USERNAME=user
SMTP_PASSWORD=password
SMTP_SENDER=alerts@example.com
```

### Slack Integration

Configure Slack notifications:

```env
SLACK_CLIENT_ID=your-client-id
SLACK_CLIENT_SECRET=your-secret
SLACK_BOT_TOKEN=xoxb-your-bot-token
SLACK_REDIRECT_URI=http://localhost:8080/slack/oauth/callback/
```

### Discord Integration

Configure Discord notifications:

```env
DISCORD_CLIENT_ID=your-client-id
DISCORD_CLIENT_SECRET=your-secret
DISCORD_BOT_TOKEN=your-bot-token
DISCORD_WEBHOOK_URL=https://discord.com/api/webhooks/...
```

## How Anomaly Detection Works

The AI anomaly detection system continuously monitors your:

1. **Logs**: Identifies unusual error patterns and new error types
2. **Metrics**: Detects deviations from normal performance baselines
3. **Traces**: Finds unusual latency patterns and failed request paths

The system needs approximately 24-48 hours of data to establish baseline patterns.

## Daily and Weekly Reports

Monoscope can send automated summary reports to keep your team informed:

### Configuration

Enable automated reports in your environment:

```env
ENABLE_DAILY_JOB_SCHEDULING=True
```

### Report Contents

Reports include:
- Summary of anomalies detected
- System health overview
- Key metrics and trends
- Notable events and errors

### Setting Up Reports

Configure report preferences in your project settings:
- Choose daily or weekly frequency
- Select email recipients
- Set preferred delivery time

## Alert Management

### Viewing Anomalies

1. Navigate to the "Anomalies" section in your project
2. View detected anomalies with:
   - Severity level
   - Affected services
   - Time of detection
   - Related log entries

### Alert States

Anomalies can be in different states:
- **New**: Recently detected, not yet reviewed
- **Acknowledged**: Marked as seen by team member
- **Resolved**: Issue has been fixed

## Best Practices

### Reducing Alert Fatigue

1. **Trust the AI**: Let the anomaly detection learn your patterns
2. **Review regularly**: Check anomalies daily rather than reacting to each one
3. **Use reports**: Weekly reports provide better context than individual alerts
4. **Mark false positives**: Help train the system by providing feedback

### Effective Response

When an anomaly is detected:
1. Review the anomaly details and related logs
2. Check if it correlates with recent deployments or changes
3. Acknowledge the anomaly if you're investigating
4. Document resolution for future reference

## Configuration Tips

### For Development Environments

- Consider disabling alerts to avoid noise from testing
- Use lower sensitivity settings

### For Production

- Ensure notification channels are properly configured
- Test notification delivery before relying on alerts
- Set up multiple notification channels for redundancy

## Troubleshooting

### Not Seeing Anomalies

- Ensure data is being ingested properly
- Wait 24-48 hours for the AI to learn patterns
- Check that background jobs are enabled:
  ```env
  ENABLE_BACKGROUND_JOBS=True
  ```

### Not Receiving Notifications

- Verify email/Slack/Discord credentials are correct
- Check spam folders for email notifications
- Ensure notification services are enabled in configuration
- Test with manual notification send

### Too Many False Positives

- The system improves over time as it learns
- Mark false positives to help train the model
- Consider adjusting project settings for your use case

## Limitations

- Anomaly detection requires consistent data ingestion
- New services need time to establish baselines
- Major architectural changes may require relearning patterns
- Email notifications require configured email service (SendGrid or SMTP)