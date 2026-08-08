(()=>{
    //========================================================
    // htmx 2.0 compatibility extension
    //========================================================
    let api

    // LOCAL PATCH (monoscope): htmx 2 listeners read `detail.elt` (hyperscript's own
    // htmx:load handler calls processNode(detail.elt)) and `detail.target` (chart
    // teardown, dashboard tab handlers). htmx 4's payload carries neither, so replaying
    // the old event names verbatim handed those listeners `undefined` and they threw.
    // Backfill both from the element the hook fired on before re-triggering.
    // Re-apply when bumping htmx: upstream ships this file without the two lines.
    function maybeRetriggerEvent(elt, evtName, detail) {
        if (!htmx.config.compat?.doNotTriggerOldEvents) {
            if (detail && typeof detail === 'object') {
                if (detail.elt === undefined) detail.elt = elt;
                if (detail.target === undefined) detail.target = elt;
            }
            htmx.trigger(elt, evtName, detail);
        }
    }

    htmx.registerExtension('compat', {
        init: (internalAPI) => {
            api = internalAPI;

            // revert inheritance
            if (!htmx.config.compat?.useExplicitInheritace) {
                htmx.config.implicitInheritance = true;
            }

            // do not swap 4xx and 5xx responses
            if (!htmx.config.compat?.swapErrorResponseCodes) {
                htmx.config.noSwap.push("4xx", "5xx");
            }
        },
        // Re-delegate new events to old event names for backwards compatibility
        htmx_after_implicitInheritance: function (elt, detail) {
            if (!htmx.config.compat?.suppressInheritanceLogs) {
                console.log("IMPLICIT INHERITANCE DETECTED, attribute: " + detail.name + ", elt: ", elt, ", inherited from: ", detail.parent)
                let evt = new CustomEvent("htmxImplicitInheritace", {
                    detail,
                    cancelable: true,
                    bubbles : true,
                    composed: true,
                });
                elt.dispatchEvent(evt)
            }
        },
        htmx_after_init: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:afterOnLoad", detail);
            maybeRetriggerEvent(elt, "htmx:afterProcessNode", detail);
            maybeRetriggerEvent(elt, "htmx:load", detail);
        },
        htmx_after_request: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:afterRequest", detail);
        },
        htmx_after_swap: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:afterSettle", detail);
            maybeRetriggerEvent(elt, "htmx:afterSwap", detail);
        },
        htmx_before_cleanup: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeCleanupElement", detail);
        },
        htmx_before_history_update: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeHistoryUpdate", detail);
            maybeRetriggerEvent(elt, "htmx:beforeHistorySave", detail);
        },
        htmx_before_init: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeOnLoad", detail);
        },
        htmx_before_process: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeProcessNode", detail);
        },
        htmx_before_request: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeRequest", detail);
            maybeRetriggerEvent(elt, "htmx:beforeSend", detail);
        },
        htmx_before_swap: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeSwap", detail);
        },
        htmx_before_viewTransition: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:beforeTransition", detail);
        },
        htmx_config_request: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:configRequest", detail);
        },
        htmx_before_history_restore: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:historyRestore", detail);
        },
        htmx_after_history_push: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:pushedIntoHistory", detail);
        },
        htmx_after_history_replace: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:replacedInHistory", detail);
        },
        htmx_error: function (elt, detail) {
            maybeRetriggerEvent(elt, "htmx:targetError", detail);
        },
    });
})();
