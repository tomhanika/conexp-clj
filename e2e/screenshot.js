// Headless-browser harness for the conexp-clj web GUI.
//
// Loads the running SPA in the system Chromium, drives the core flow, saves
// screenshots to e2e/shots/, and reports console/page errors + basic DOM facts.
// The API server must already be serving the SPA:
//     lein run -a [-p PORT]
// then:
//     E2E_URL=http://127.0.0.1:PORT npm run e2e
//
// Screenshots (e2e/shots/*.png) are inspected to judge visual quality; they are
// not committed.

const fs = require("fs");
const path = require("path");
const puppeteer = require("puppeteer-core");

const URL = process.env.E2E_URL || process.argv[2] || "http://127.0.0.1:8080";
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const OUT = path.join(__dirname, "shots");
fs.mkdirSync(OUT, { recursive: true });

const shot = (page, name) => page.screenshot({ path: path.join(OUT, name + ".png"), fullPage: false });

(async () => {
  const errors = [];
  const browser = await puppeteer.launch({
    executablePath: CHROME,
    headless: "new",
    args: ["--no-sandbox", "--disable-setuid-sandbox", "--disable-dev-shm-usage"],
  });
  try {
    const page = await browser.newPage();
    await page.setViewport({ width: 1200, height: 820 });
    page.on("console", (m) => { if (m.type() === "error") errors.push("console: " + m.text()); });
    page.on("pageerror", (e) => errors.push("pageerror: " + e.message));
    page.on("requestfailed", (r) => errors.push("requestfailed: " + r.url() + " " + (r.failure() && r.failure().errorText)));

    console.log("URL", URL);
    await page.goto(URL, { waitUntil: "networkidle2", timeout: 30000 });

    await page.waitForFunction(() => document.body.innerText.includes("Show Concept Lattice"), { timeout: 15000 });
    await shot(page, "1-editor");

    // load a built-in example (tests dataset loading + a non-trivial lattice)
    const loaded = await page.evaluate(() => {
      const sel = document.querySelector("select");
      if (!sel || sel.options.length < 2) return false;
      sel.value = "0";
      sel.dispatchEvent(new Event("change", { bubbles: true }));
      return true;
    });
    console.log("EXAMPLE_LOADED", loaded);
    await new Promise((r) => setTimeout(r, 400));
    await shot(page, "1b-editor-example");

    await page.evaluate(() => {
      [...document.querySelectorAll("button")].find((b) => b.textContent.includes("Show Concept Lattice")).click();
    });
    await page.waitForSelector("svg circle", { timeout: 15000 });
    await new Promise((r) => setTimeout(r, 600));
    await shot(page, "2-diagram");

    // measure the rendered diagram: node circles vs. the svg viewport, to catch
    // "nodes too big / unreadable"
    const geom = await page.evaluate(() => {
      const svg = document.querySelector("svg");
      const vb = svg.viewBox.baseVal;
      const rs = [...svg.querySelectorAll("circle")].map((c) => c.r.baseVal.value);
      const r = rs.length ? rs.sort((a, b) => a - b)[Math.floor(rs.length / 2)] : null;
      return {
        nodes: [...svg.querySelectorAll("circle")].length / 2, // 2 circles per node
        edges: svg.querySelectorAll("line").length,
        medianRadius: r,
        viewBox: [vb.width, vb.height],
        radiusVsViewbox: r ? (r / Math.max(vb.width, vb.height)) : null,
      };
    });
    console.log("DIAGRAM", JSON.stringify(geom));

    // toggle labels off (declutter) and confirm labels disappear
    const before = await page.$$eval("svg text", (t) => t.length);
    await page.evaluate(() => {
      const cb = [...document.querySelectorAll('input[type="checkbox"]')].pop();
      if (cb) cb.click();
    });
    await new Promise((r) => setTimeout(r, 300));
    const after = await page.$$eval("svg text", (t) => t.length);
    console.log("LABELS_TOGGLE", JSON.stringify({ withLabels: before, withoutLabels: after }));
    await shot(page, "3-nolabels");

    console.log("CONSOLE_ERRORS", JSON.stringify(errors));
    console.log("SHOTS", OUT);
    console.log(errors.length ? "E2E_FAIL" : "E2E_OK");
  } catch (e) {
    console.error("E2E_ERROR", e.message);
    process.exitCode = 1;
  } finally {
    await browser.close();
  }
})();
