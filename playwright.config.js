const { defineConfig } = require('@playwright/test');

module.exports = defineConfig({
  workers: 1,
  use: {
    channel: 'chrome',
    headless: true,
  },
});
