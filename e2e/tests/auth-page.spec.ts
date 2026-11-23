import { test, expect } from "@playwright/test";

// The public log-in / sign-up page. Auth0 is not configured on the e2e server, so these check
// what the page hands to /login/start rather than following the redirect off-site.

test("log-in page offers GitHub, Google and email, each carrying redirect_to", async ({ page }) => {
  await page.goto("/login?redirect_to=/p/x/log_explorer");
  await expect(page.getByRole("heading", { name: "Welcome back" })).toBeVisible();
  await expect(page.getByRole("link", { name: "Continue with GitHub" })).toHaveAttribute("href", "/login/start?redirect_to=%2Fp%2Fx%2Flog_explorer&connection=github");
  await expect(page.getByRole("link", { name: "Continue with Google" })).toHaveAttribute("href", /connection=google-oauth2/);
  await expect(page.locator('form[action="/login/start"] input[name="redirect_to"]')).toHaveValue("/p/x/log_explorer");
  await expect(page.locator("html")).toHaveAttribute("data-theme", "light");
});

test("invite links open sign-up with the email filled in", async ({ page }) => {
  await page.goto("/login?screen_hint=signup&login_hint=invitee@example.com");
  await expect(page.getByRole("heading", { name: /Start free/ })).toBeVisible();
  await expect(page.getByLabel("Work email")).toHaveValue("invitee@example.com");
  await expect(page.locator('form input[name="screen_hint"]')).toHaveValue("signup");
  await expect(page.getByRole("link", { name: "Sign up with GitHub" })).toHaveAttribute("href", /screen_hint=signup/);
  await page.getByRole("link", { name: "Log in" }).click();
  await expect(page.getByRole("heading", { name: "Welcome back" })).toBeVisible();
});

test("a failed Auth0 callback explains itself instead of a silent loop", async ({ page }) => {
  await page.goto("/login?auth0_callback_failure");
  await expect(page.getByRole("alert")).toContainText("couldn't finish signing you in");
});

test("/login/start hands the connection and hints to Auth0", async ({ request }) => {
  const res = await request.get("/login/start?connection=github&login_hint=a@b.com&screen_hint=signup", { maxRedirects: 0 });
  expect(res.status()).toBe(302);
  const location = res.headers()["location"];
  for (const part of ["/authorize?", "&connection=github", "&login_hint=a%40b.com", "&screen_hint=signup"]) expect(location).toContain(part);
});

test("a dark-theme cookie gets the dark page, and phones get the form without the art", async ({ page }) => {
  await page.goto("/login");
  await expect(page.locator('img[src*="sky-morning"]')).toBeVisible();
  await page.evaluate(() => (document.cookie = "theme=dark; path=/"));
  await page.reload();
  await expect(page.locator("html")).toHaveAttribute("data-theme", "dark");
  await expect(page.locator('img[src*="sky-night"]')).toBeVisible();

  await page.setViewportSize({ width: 375, height: 800 });
  await expect(page.locator("aside")).toBeHidden();
  await expect(page.getByRole("link", { name: "Continue with GitHub" })).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
});
