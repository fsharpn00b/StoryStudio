import { expect, test, type Page } from "@playwright/test";

async function dispatchWindowKey(page: Page, key: string) {
  await page.evaluate((keyToDispatch) => {
    window.dispatchEvent(new KeyboardEvent("keydown", { key: keyToDispatch, bubbles: true }));
  }, key);
}

async function waitForSaveLoadHeader(page: Page, expectedHeaderText: string) {
  await expect(page.locator("#save_load_screen")).toBeVisible({ timeout: 15000 });
  await expect
    .poll(
      async () => {
        const headerText = await page.locator("#save_load_header").innerText();
        return headerText.trim();
      },
      { timeout: 15000 }
    )
    .toContain(expectedHeaderText);
}

async function advanceUntilDialogueContains(page: Page, expectedText: string, maxSteps = 30) {
  for (let step = 0; step < maxSteps; step += 1) {
    const currentText = (await page.locator("#dialogue_text").textContent()) ?? "";
    if (currentText.includes(expectedText)) {
      return;
    }
    await page.click("body");
    await page.waitForTimeout(120);
  }
  await expect(page.locator("#dialogue_text")).toContainText(expectedText, { timeout: 15000 });
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

test("save-load switching works while menu is open", async ({ page }) => {
  await expect(page.locator("#dialogue_text")).toContainText("Which way should we go?", {
    timeout: 30000,
  });

  await page.click("body");
  await expect(page.locator("#menu_container")).toBeVisible({ timeout: 15000 });

  await dispatchWindowKey(page, "s");
  await waitForSaveLoadHeader(page, "Save Game");

  await dispatchWindowKey(page, "l");
  await waitForSaveLoadHeader(page, "Load Game");

  await dispatchWindowKey(page, "Escape");
  await expect(page.locator("#save_load_screen")).toBeHidden({ timeout: 10000 });
  await expect(page.locator("#menu_container")).toBeVisible({ timeout: 10000 });
});

test("alternate branch reaches day 2 scene", async ({ page }) => {
  await expect(page.locator("#dialogue_text")).toContainText("Which way should we go?", {
    timeout: 30000,
  });

  await page.click("body");
  await expect(page.locator("#menu_container")).toBeVisible({ timeout: 15000 });
  await page.locator("#menu .menu_item").first().click();

  await advanceUntilDialogueContains(page, "Great, we're lost in the woods.");
});
