// How a failed request reads to the reader, shared by the log list, its worker, charts and
// dashboard variables. Dependency-free so the worker can import it.

// A failure the reader is already shown (a rejected query, a lapsed session, an HTTP error, a
// dropped connection). Warned, never console.error'd: the browser SDK files those as exceptions.
export class ShownFailure extends Error {}
export const logFailure = (...args: unknown[]) => (args.some((a) => a instanceof ShownFailure) ? console.warn : console.error)(...args);
export const asShown = (e: unknown): never => {
  throw e instanceof ShownFailure ? e : new ShownFailure((e instanceof Error && e.message) || 'Network error');
};

// An empty or truncated body (a dropped connection, a restarting server), which response.json()
// reports as "Unexpected end of JSON input".
export const incompleteResponse = (status: number) => `The server sent an incomplete response (HTTP ${status}). Retry to load events.`;

const httpFailure = (status: number) =>
  status === 401 ? 'Session expired, please refresh' : [504, 524].includes(status) ? 'The query timed out. Try a shorter time range or a narrower filter.' : `Request failed (HTTP ${status})`;

// The JSON body, including a server-reported `error` the caller routes; every other failure is a
// ShownFailure. Accept keeps an expired session answering 401 rather than redirecting to the login page.
export async function fetchJson<T>(url: string): Promise<T> {
  const resp = await fetch(url, { headers: { Accept: 'application/json' }, credentials: 'include' }).catch(asShown);
  const body = await resp.json().catch(() => undefined);
  // A lapsed session also answers {error: "unauthenticated"}; it is not a query error to route.
  if (resp.status === 401 || (!resp.ok && body?.error === undefined)) throw new ShownFailure(httpFailure(resp.status));
  if (body == null) throw new ShownFailure(incompleteResponse(resp.status));
  return body;
}
