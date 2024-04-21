export const PostConfig = {
  method: "POST",
  headers: { "Content-Type": "application/json" },
};

export function getEvent(eventName, value) {
  const event = new CustomEvent(eventName, {
    detail: value,
    bubbles: true,
    composed: true,
  });
  return event;
}

export const METHODS = [
  "GET",
  "POST",
  "PATCH",
  "PUT",
  "DELETE",
  "HEAD",
  "OPTIONS",
];
export const ASSERTS = [
  "exists",
  "number",
  "string",
  "boolean",
  "ok",
  "empty",
  "notEmpty",
];

export function triggerToastEvent(event) {
  document.querySelector("body").dispatchEvent(event);
}

function isValidStep(step, step_ind, errors) {
  hasMethod(step, step_ind, errors);
  if (step.asserts) isValidAssert(step.asserts, step_ind, errors);
  if (step.headers) hasValidHeaders(step.headers, step_ind, errors);
}
function hasValidHeaders(headers, step, errors) {
  // Check if headers is an object
  if (
    typeof headers !== "object" ||
    headers === null ||
    Array.isArray(headers)
  ) {
    errors.push(`STEP ${step} (Headers): Header must be key value object pair`);
    return;
  }

  for (const [key, value] of Object.entries(headers)) {
    if (typeof key !== "string" || key.trim() === "") {
      errors.push(`STEP ${step} (Headers): ${key} is not a valid header`);
      return;
    }
    if (typeof value !== "string") {
      errors.push(`STEP ${step} (Headers): Value ${key} must be a string`);
      return;
    }
  }
  return true;
}

function hasMethod(obj, step, errors) {
  if (typeof obj !== "object" || obj === null || Array.isArray(obj)) {
    errors.push(`STEP ${step}: No valid http method found`);
    return;
  }
  const httpMethodKeys = Object.keys(obj).filter((key) =>
    METHODS.includes(key)
  );
  if (httpMethodKeys.length !== 1) {
    errors.push(`STEP ${step}: No valid HTTP method found`);
    return;
  }
  const httpMethodValue = obj[httpMethodKeys[0]];
  if (typeof httpMethodValue !== "string") {
    errors.push(`STEP ${step}: Invalid HTTP method value`);
  }
}

function isValidAssert(asserts, step, errors) {
  if (!Array.isArray(asserts)) {
    errors.push(`STEP ${step} (Asserts): Asserts must be an array`);
    return;
  }
  for (const item of asserts) {
    if (typeof item !== "object" || item === null || Array.isArray(item)) {
      errors.push(`STEP ${step} (Asserts): Each assert must be an object`);
      return;
    }
    const keys = Object.keys(item);
    if (keys.length !== 1) {
      errors.push(`STEP ${step} (Asserts): Each assert must have only one key`);
      return;
    }

    if (!ASSERTS.includes(keys[0])) {
      errors.push(
        `STEP ${step} (Asserts): ${keys[0]} is not a valid assertion `
      );
    }
  }
}

export function validateYaml(yaml) {
  try {
    const data = jsyaml.load(yaml);
    if (!Array.isArray(data)) {
      triggerToastEvent(
        getEvent("errorToast", { value: ["Array of steps expected"] })
      );
      return undefined;
    }
    let errors = [];
    for (let [ind, step] of data.entries()) {
      isValidStep(step, ind + 1, errors);
      if (errors.length !== 0) {
        return {
          validate_errors: errors,
        };
      }
    }
    data.map((step) => {
      if (step.json) {
        step.json =
          typeof step.json === "string" ? step.json : JSON.stringify(step.json);
      }
    });
    return data;
  } catch (error) {
    const event = getEvent("errorToast", { value: ["Invalid yaml"] });
    triggerToastEvent(event);
    return undefined;
  }
}

export function getDeletedUpdatedAndNewSteps(steps, newSteps) {
  const deletedSteps = steps
    .filter((step) => !newSteps.some((s) => s.id === step.id))
    .map((step) => step.id);
  const addedSteps = newSteps.filter((step) => !step.id);
  const updatedSteps = newSteps
    .filter((step) => {
      const updatedStep = steps.find((s) => s.id === step.id);
      return (
        updatedStep && JSON.stringify(step) !== JSON.stringify(updatedStep)
      );
    })
    .map((step) => {
      const id = step.id;
      delete step.id;
      delete step.lastRun;
      return { stepId: id, stepData: step };
    });
  return { deletedSteps, updatedSteps, addedSteps };
}
