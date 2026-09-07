const { test, expect } = require('@playwright/test');
const path = require('node:path');
const deps = path.resolve(__dirname, '../../static/public/assets/deps/htmx');

for (const mode of ['post', 'get', 'preload']) {
  test(`${mode} preserves hx-vals arrays as repeated form fields`, async ({ page }) => {
    const requests = [];
    await page.route('http://notification.test/**', async route => {
      const req = route.request();
      if (new URL(req.url()).pathname === '/save') requests.push(req);
      await route.fulfill({ status: 200, contentType: 'text/html', body: '<html><body></body></html>' });
    });
    await page.goto('http://notification.test/');
    for (const file of ['htmx-4.0.0-beta6.min.js', 'htmx-2-compat.js', 'hx-preload-4.js']) {
      await page.addScriptTag({ path: path.join(deps, file) });
    }
    await page.setContent(`<body hx-preload:inherited="mouseover">
      <input name="notifChannel" type="checkbox" value="email" checked>
      <input name="notifChannel" type="checkbox" value="slack" checked>
      <button id="save" hx-preload="mouseover" hx-${mode === 'post' ? 'post' : 'get'}="/save" hx-swap="none"
        hx-vals='js:{enabledChannels: Array.from(document.querySelectorAll("input[name=notifChannel]:checked")).map(i => i.value), emails: ["a@example.com", "b@example.com"], slackChannels: ["C_ONE", "C_TWO"], phones: [], scalar: "keep,comma"}'>Save</button>
      </body>`);
    await page.evaluate(() => htmx.process(document.body));
    await page.locator('#save').hover();
    if (mode === 'post') expect(requests).toHaveLength(0);
    if (mode !== 'preload') await page.locator('#save').click();
    await expect.poll(() => requests.length).toBeGreaterThan(0);
    const request = requests[0];
    expect(request.method()).toBe(mode === 'post' ? 'POST' : 'GET');
    const params = mode === 'post' ? new URLSearchParams(request.postData()) : new URL(request.url()).searchParams;
    expect(params.getAll('enabledChannels')).toEqual(['email', 'slack']);
    expect(params.getAll('emails')).toEqual(['a@example.com', 'b@example.com']);
    expect(params.getAll('slackChannels')).toEqual(['C_ONE', 'C_TWO']);
    expect(params.has('phones')).toBe(false);
    expect(params.get('scalar')).toBe('keep,comma');
  });
}
