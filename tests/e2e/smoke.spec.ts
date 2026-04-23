import { expect, test, type Page } from "@playwright/test";

async function dispatchWindowKey(page: Page, key: string) {
  await page.evaluate((keyToDispatch) => {
    window.dispatchEvent(new KeyboardEvent("keydown", { key: keyToDispatch, bubbles: true }));
  }, key);
}

test.beforeEach(async ({ page }) => {
  await page.goto("/0_pages/index.html");
  await page.evaluate(() => {
    window.localStorage.clear();
  });
  await page.reload();
});

test("plays through first menu branch and keeps progressing", async ({ page }) => {
  await expect(page.locator("#dialogue_text")).toContainText("Which way should we go?", {
    timeout: 30000,
  });

  await page.click("body");
  await expect(page.locator("#menu_container")).toBeVisible({ timeout: 15000 });
  await page.locator("#menu .menu_item").last().click();
  await expect(page.locator("#menu_container")).toBeHidden({ timeout: 15000 });
  await expect(page.locator("#dialogue_text")).toContainText("Great idea!", {
    timeout: 15000,
  });
});

test("save and close via keyboard", async ({ page }) => {
  await expect(page.locator("#dialogue_text")).toContainText("Which way should we go?", {
    timeout: 30000,
  });
  await dispatchWindowKey(page, "s");
  await expect(page.locator("#save_load_screen")).toBeVisible({ timeout: 10000 });
  await dispatchWindowKey(page, "Escape");
  await expect(page.locator("#save_load_screen")).toBeHidden({ timeout: 10000 });
});
