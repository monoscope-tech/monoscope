export const PostConfig = {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
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
  'GET',
  'POST',
  'PATCH',
  'PUT',
  'DELETE',
  'HEAD',
  'OPTIONS',
];
export const ASSERTS = [
  'exists',
  'number',
  'string',
  'boolean',
  'ok',
  'empty',
  'notEmpty',
];

export function triggerToastEvent(event) {
  document.querySelector('body').dispatchEvent(event);
}

function isValidStep(step) {
  if (!isValidAssert(step.asserts)) {
    return false;
  }
  if (!hasMethod(step)) {
    return false;
  }
  if (!hasValidHeaders) {
    return false;
  }
  return true;
}
function hasValidHeaders(headers) {
  // Check if headers is an object
  if (
    typeof headers !== 'object' ||
    headers === null ||
    Array.isArray(headers)
  ) {
    return false;
  }

  for (const [key, value] of Object.entries(headers)) {
    if (typeof key !== 'string' || key.trim() === '') {
      return false;
    }
    if (typeof value !== 'string') {
      return false;
    }
  }
  return true;
}

function hasMethod(obj) {
  if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
    return false;
  }
  const httpMethodKeys = Object.keys(obj).filter((key) =>
    METHODS.includes(key)
  );
  if (httpMethodKeys.length !== 1) {
    return false;
  }
  const httpMethodValue = obj[httpMethodKeys[0]];
  return typeof httpMethodValue === 'string';
}

function isValidAssert(asserts) {
  if (!Array.isArray(asserts)) {
    return false;
  }
  for (const item of asserts) {
    if (typeof item !== 'object' || item === null || Array.isArray(item)) {
      return false;
    }
    const keys = Object.keys(item);
    if (keys.length !== 1) {
      return false;
    }

    if (!ASSERTS.includes(keys[0])) {
      return false;
    }
  }
  return true;
}

export function validateYaml(yaml) {
  try {
    const data = jsyaml.load(yaml);
    if (!Array.isArray(data)) {
      triggerToastEvent(
        getEvent('errorToast', { value: ['Array of steps expected'] })
      );
      return undefined;
    }
    for (let [ind, step] of data.entries()) {
      if (!isValidStep(step)) {
        const event = getEvent('errorToast', {
          value: [`Step ${ind} is invalid`],
        });
        triggerToastEvent(event);
        return undefined;
      }
    }
    data.map((step) => {
      if (step.json) {
        step.json =
          typeof step.json === 'string' ? step.json : JSON.stringify(step.json);
      }
    });
    return data;
  } catch (error) {
    console.log(error);
    const event = getEvent('errorToast', { value: ['Invalid yaml'] });
    triggerToastEvent(event);
    return undefined;
  }
}

export function getDeletedUpdatedAndNewSteps(steps, newSteps) {
  const deletedSteps = steps.filter(
    (step) => !newSteps.some((s) => s.id === step.id)
  );
  const updatedSteps = steps.filter((step) => {
    const newStep = newSteps.find((s) => s.id === step.id);
    return newStep && JSON.stringify(step) !== JSON.stringify(newStep);
  });
  const addedSteps = newSteps.filter((step) => !step.id);
  return { deletedSteps, updatedSteps, addedSteps };
}
