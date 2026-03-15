#!/usr/bin/env node
"use strict";

const { execFileSync } = require("child_process");
const os = require("os");
const path = require("path");

const PLATFORMS = {
  "linux-x64": "@monoscope/cli-linux-x64",
  "darwin-arm64": "@monoscope/cli-darwin-arm64",
  "darwin-x64": "@monoscope/cli-darwin-x64",
  "win32-x64": "@monoscope/cli-win32-x64",
};

function getBinaryPath() {
  const key = `${os.platform()}-${os.arch()}`;
  const pkg = PLATFORMS[key];
  if (!pkg) {
    console.error(`Unsupported platform: ${key}`);
    console.error(`Supported: ${Object.keys(PLATFORMS).join(", ")}`);
    process.exit(1);
  }
  try {
    const pkgDir = path.dirname(require.resolve(`${pkg}/package.json`));
    const bin = os.platform() === "win32" ? "monoscope.exe" : "monoscope";
    return path.join(pkgDir, "bin", bin);
  } catch {
    console.error(`Platform package ${pkg} not installed.`);
    console.error(`Run: npm install ${pkg}`);
    process.exit(1);
  }
}

try {
  execFileSync(getBinaryPath(), process.argv.slice(2), { stdio: "inherit" });
} catch (e) {
  if (e.status !== undefined) process.exit(e.status);
  throw e;
}
