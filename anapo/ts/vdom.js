var VDom;
(function (VDom) {
    // ------------------------------------------------------------------
    // types
    // utils
    // ------------------------------------------------------------------
    function callLifecycleCallback(dom, callbacks, callback) {
        if (callbacks && callbacks[callback]) {
            callbacks[callback].callback(dom); // why do we need ! here?
        }
    }
    VDom.callLifecycleCallback = callLifecycleCallback;
    // we need these because we cannot easily construct object literals
    // from haskell, because the closure compiler is prone to renaming
    // their fields
    function newElement(tag) {
        return { tag: tag, style: {}, properties: {}, attributes: {}, classes: {}, events: [], children: {} };
    }
    VDom.newElement = newElement;
    function newKeyedChildren() {
        return { order: [], elements: {} };
    }
    VDom.newKeyedChildren = newKeyedChildren;
    function newEventCallback(type_, callback, token) {
        return { type: type_, callback: callback, token: token };
    }
    VDom.newEventCallback = newEventCallback;
})(VDom || (VDom = {}));
var Render;
(function (Render) {
    function stringEq(prop1, prop2) {
        if (typeof prop1 === "string" && typeof prop2 === "string") {
            return prop1 === prop2;
        }
        return false;
    }
    // modifies the defProps in place.
    function addProperties(defProps, el, props, prevProps) {
        // remove properties that are not there anymore
        for (var prop in prevProps) {
            if (!props[prop]) {
                el[prop] = defProps[prop];
                delete defProps[prop];
            }
        }
        // add the new ones
        for (var prop in props) {
            if (prevProps[prop]) {
                defProps[prop] = defProps[prop];
                // if the previous properties had the prop already, set the property unless
                // they're the same
                if (!stringEq(props[prop], prevProps[prop])) {
                    el[prop] = props[prop];
                }
            }
            else {
                // if it was not in the previous one, store the default property
                var defProp = el[prop];
                defProps[prop] = defProp;
                el[prop] = props[prop];
            }
        }
    }
    function addAttributes(dom, attrs, prevAttrs) {
        // remove all the attrs that are not there anymore
        for (var attr in prevAttrs) {
            if (!attrs[attr]) {
                dom.removeAttribute(attr);
            }
        }
        // add the others
        for (var attr in attrs) {
            // only set if they're not equal
            if (!prevAttrs[attr] || !stringEq(prevAttrs[attr], attrs[attr])) {
                dom.setAttribute(attr, attrs[attr]);
            }
        }
    }
    function addStyle(dom, style, prevStyle) {
        var css = dom.style;
        // remove all the keys that are not there anymore
        for (var k in prevStyle) {
            if (!style[k]) {
                css.removeProperty(k);
            }
        }
        // add all the new ones
        for (var k in style) {
            // only set if they're not equal
            if (!prevStyle[k] || style[k] !== prevStyle[k]) {
                css.setProperty(k, style[k]);
            }
        }
    }
    function addClasses(dom, classes, prevClasses) {
        // note that we allow people to put spaces in classes.
        var domClasses = dom.classList;
        // remove all the classes that are not there anymore
        for (var cls in prevClasses) {
            if (!classes[cls]) {
                domClasses.remove(cls);
            }
        }
        // add the new ones
        for (var cls in classes) {
            if (!prevClasses[cls]) {
                domClasses.add(cls);
            }
        }
    }
    // appends to the tokens
    function addEvents(dom, events, oldEvents) {
        // remove the old ones
        for (var _i = 0, oldEvents_1 = oldEvents; _i < oldEvents_1.length; _i++) {
            var event_1 = oldEvents_1[_i];
            dom.removeEventListener(event_1.type, event_1.callback);
        }
        // add the new ones
        for (var _a = 0, events_1 = events; _a < events_1.length; _a++) {
            var event_2 = events_1[_a];
            dom.addEventListener(event_2.type, event_2.callback);
        }
    }
    function renderDom(container, children) {
        return children.map(function (child) { return render(child, function (rvdom) {
            container.appendChild(rvdom.dom);
            return rvdom;
        }); });
    }
    function renderKeyedDom(container, children) {
        var elements = {};
        var _loop_1 = function (key) {
            var child = children.elements[key];
            render(child, function (rvdom) {
                container.appendChild(rvdom.dom);
                elements[key] = rvdom;
            });
        };
        for (var _i = 0, _a = children.order; _i < _a.length; _i++) {
            var key = _a[_i];
            _loop_1(key);
        }
        return {
            order: children.order,
            elements: elements
        };
    }
    function renderChildren(container, children) {
        if (children.normal) {
            return { normal: renderDom(container, children.normal) };
        }
        if (children.keyed) {
            return { keyed: renderKeyedDom(container, children.keyed) };
        }
        if (children.rawHtml) {
            container.innerHTML = children.rawHtml;
            return { rawHtml: children.rawHtml };
        }
        throw "Couldn't find normal, keyed, or rawHtml";
    }
    function render(vdom, cont) {
        function finalize(rvdom) {
            VDom.callLifecycleCallback(rvdom.dom, vdom.callbacks, "willMount");
            cont(rvdom);
            VDom.callLifecycleCallback(rvdom.dom, vdom.callbacks, "didMount");
            return rvdom;
        }
        if (vdom.element) {
            var dom = document.createElement(vdom.element.tag);
            var rvdom = { vdom: vdom, dom: dom };
            rvdom.defaultProperties = {};
            addProperties(rvdom.defaultProperties, dom, vdom.element.properties, {});
            addAttributes(dom, vdom.element.attributes, {});
            addStyle(dom, vdom.element.style, {});
            addClasses(dom, vdom.element.classes, {});
            addEvents(dom, vdom.element.events, []);
            rvdom.children = renderChildren(dom, vdom.element.children);
            return finalize(rvdom);
        }
        if (vdom.text) {
            return finalize({ vdom: vdom, dom: document.createTextNode(vdom.text) });
        }
        if (vdom.raw) {
            return finalize({ vdom: vdom, dom: vdom.raw });
        }
        throw "VDOM didn't contain element, text, or raw element.";
    }
    Render.render = render;
    function releaseLifecycleCallback(callbacksToRelease, callbacks, callback) {
        if (callbacks && callbacks[callback]) {
            callbacksToRelease.push(callback);
        }
    }
    function releaseLifecycleCallbacks(callbacksToRelease, callbacks) {
        releaseLifecycleCallback(callbacksToRelease, callbacks, "willMount");
        releaseLifecycleCallback(callbacksToRelease, callbacks, "didMount");
        releaseLifecycleCallback(callbacksToRelease, callbacks, "willPatch");
        releaseLifecycleCallback(callbacksToRelease, callbacks, "didPatch");
        releaseLifecycleCallback(callbacksToRelease, callbacks, "willRemove");
    }
    // appends to callbacksToRelease
    function cleanupNode(rvdom, callbacksToRelease) {
        // call the callback
        VDom.callLifecycleCallback(rvdom.dom, rvdom.vdom.callbacks, "willRemove");
        // recurse down
        if (rvdom.children) {
            cleanupChildren(rvdom.children, callbacksToRelease);
        }
        // put the tokens for the callbacks and event listeners
        releaseLifecycleCallbacks(callbacksToRelease, rvdom.vdom.callbacks);
        if (rvdom.vdom.element) {
            for (var _i = 0, _a = rvdom.vdom.element.events; _i < _a.length; _i++) {
                var event_3 = _a[_i];
                callbacksToRelease.push(event_3.token);
            }
        }
    }
    // appends to callbacksToRelease
    function cleanupChildren(rchildren, callbacksToRelease) {
        if (rchildren.rawHtml) {
            // nothing to do
        }
        else if (rchildren.normal) {
            rchildren.normal.forEach(function (rvdom) { return cleanupNode(rvdom, callbacksToRelease); });
        }
        else if (rchildren.keyed) {
            for (var _i = 0, _a = rchildren.keyed.order; _i < _a.length; _i++) {
                var k = _a[_i];
                cleanupNode(rchildren.keyed.elements[k], callbacksToRelease);
            }
        }
        else {
            throw "Could not find normal, keyed, or raw html";
        }
    }
    // appends to callbacksToRelease
    function removeNode(container, rvdom, callbacksToRelease) {
        // call the callback first
        VDom.callLifecycleCallback(container, rvdom.vdom.callbacks, "willRemove");
        // cleanup the children
        if (rvdom.children) {
            cleanupChildren(rvdom.children, callbacksToRelease);
        }
        // remove DOM element
        container.removeChild(rvdom.dom);
        // put the tokens for the callbacks and event listeners
        releaseLifecycleCallbacks(callbacksToRelease, rvdom.vdom.callbacks);
        if (rvdom.vdom.element) {
            for (var _i = 0, _a = rvdom.vdom.element.events; _i < _a.length; _i++) {
                var event_4 = _a[_i];
                callbacksToRelease.push(event_4.token);
            }
        }
    }
    // removes all elements of rvdomChildren past the cursor
    function removeDom(domEl, rvdomChildren, callbacksToRelease, cursor) {
        for (var i = cursor; i < rvdomChildren.length; i++) {
            var rvdom = rvdomChildren[i];
            removeNode(domEl, rvdomChildren[i], callbacksToRelease);
        }
        rvdomChildren.length = cursor;
    }
    // modifies container and rvdomChildren, appends to callbacksToRelease
    function patchDom(container, rvdomChildren, callbacksToRelease, vdomChildren) {
        // first patch the common ones
        for (var i = 0; i < Math.min(rvdomChildren.length, vdomChildren.length); i++) {
            patchNode(rvdomChildren[i], callbacksToRelease, vdomChildren[i]);
        }
        // then, if we have more new ones, render them and push them
        if (vdomChildren.length > rvdomChildren.length) {
            rvdomChildren.push.apply(rvdomChildren, renderDom(container, vdomChildren.slice(rvdomChildren.length)));
        }
        // on the other hand if we have leftovers, remove them
        if (rvdomChildren.length > vdomChildren.length) {
            removeDom(container, rvdomChildren, callbacksToRelease, vdomChildren.length);
        }
    }
    // if the cursor is null, the node will be inserted at the beginning.
    function appendAfter(container, cursor, node) {
        if (cursor) {
            // if the cursor has no next sibling it'll append it at the end, which is what
            // we want.
            container.insertBefore(node, cursor.nextSibling);
        }
        else {
            // if the cursor is null, put it before the first element. note that
            // if there is no first element it'll just append the node, which is fine
            // since it's the only node.
            container.insertBefore(node, container.firstChild);
        }
    }
    // TODO this is a pretty dumb implementation, use a more principled one (in terms of performance)
    function patchKeyedDom(container, rvdomChildren, callbacksToRelease, vdomChildren) {
        // assumes that there are no duplicates in both .orders.
        // * traverse the two .orders at the same time
        // * if you meet the same key, patch the node
        // * if you meet different keys:
        //   - if you can get the key from the rvdom, do it and patch the node.
        //   - if you can't, render the node and add it to the rvdom.
        //   in both cases, store the current rvdom key as skipped.
        // * when one of the two orders has finished
        var patchedChildren = {}; // these are the children from later in rvdomOrder that we already patched.
        function tryExistingChild(mbCursor, vdomKey) {
            // if the key did change, see if we can find the new key in the rvdom
            var rvdomNode = rvdomChildren.elements[vdomKey];
            if (rvdomNode) {
                // if we can, move its dom after the previous element, and the patch it
                appendAfter(container, mbCursor, rvdomNode.dom);
                patchNode(rvdomNode, callbacksToRelease, vdomChildren.elements[vdomKey]);
                patchedChildren[vdomKey] = true;
                return rvdomNode.dom;
            }
            else {
                // otherwise just render it
                var rvdom = render(vdomChildren.elements[vdomKey], function (rvdom) {
                    appendAfter(container, mbCursor, rvdom.dom);
                    rvdomChildren.elements[vdomKey] = rvdom;
                });
                return rvdom.dom;
            }
        }
        var rvdomOrder = rvdomChildren.order;
        var vdomOrder = vdomChildren.order;
        var mbCursor = null; // a cursor to keep track of where we are in the dom.
        var rvdom_i = 0;
        var vdom_i = 0;
        for (; vdom_i < vdomOrder.length && rvdom_i < rvdomOrder.length;) {
            var rvdomKey = rvdomOrder[rvdom_i];
            // if we have already used the rvdomKey, skip
            if (patchedChildren[rvdomKey]) {
                rvdom_i++;
                continue;
            }
            var vdomKey = vdomOrder[vdom_i];
            // common case: the vdom didn't change
            if (rvdomKey === vdomKey) {
                var rvdomChild = rvdomChildren.elements[rvdomKey];
                patchNode(rvdomChild, callbacksToRelease, vdomChildren.elements[rvdomKey]);
                mbCursor = rvdomChild.dom;
                rvdom_i++;
                vdom_i++;
            }
            else if (!vdomChildren.elements[rvdomKey]) {
                // if the old key does not exist at all in the old stuff, remove the current node.
                // this is to avoid shuffling around nodes when it's not needed
                removeNode(container, rvdomChildren.elements[rvdomKey], callbacksToRelease);
                rvdom_i++;
            }
            else {
                mbCursor = tryExistingChild(mbCursor, vdomKey);
                vdom_i++;
            }
        }
        // render remaining nodes
        for (; vdom_i < vdomOrder.length; vdom_i++) {
            var vdomKey = rvdomOrder[vdom_i];
            mbCursor = tryExistingChild(mbCursor, vdomOrder[vdom_i]);
        }
        // Remove the leftover nodes
        for (; rvdom_i < rvdomOrder.length; rvdom_i++) {
            var rvdomKey = rvdomChildren.order[rvdom_i];
            if (!patchedChildren[rvdomKey]) {
                removeNode(container, rvdomChildren.elements[rvdomKey], callbacksToRelease);
                delete rvdomChildren.elements[rvdomKey];
            }
        }
        // set the new order
        rvdomChildren.order = vdomOrder;
    }
    function removeChildren(container, callbacksToRelease, rvdomChildren) {
        if (rvdomChildren.rawHtml) {
            container.innerHTML = "";
        }
        else if (rvdomChildren.normal) {
            removeDom(container, rvdomChildren.normal, callbacksToRelease, 0);
        }
        else if (rvdomChildren.keyed) {
            for (var _i = 0, _a = rvdomChildren.keyed.order; _i < _a.length; _i++) {
                var key = _a[_i];
                removeNode(container, rvdomChildren.keyed.elements[key], callbacksToRelease);
            }
            rvdomChildren.keyed.order = [];
            rvdomChildren.keyed.elements = {};
        }
        else {
            throw "Could not find raw html, normal, or keyed";
        }
    }
    // modifies domEl and rvdomChildren, appends to callbacksToRelease
    function patchChildren(container, rvdomChildren, callbacksToRelease, vdom) {
        if (rvdomChildren.normal && vdom.normal) {
            patchDom(container, rvdomChildren.normal, callbacksToRelease, vdom.normal);
        }
        else if (rvdomChildren.keyed && vdom.keyed) {
            patchKeyedDom(container, rvdomChildren.keyed, callbacksToRelease, vdom.keyed);
        }
        else if (rvdomChildren.rawHtml && vdom.rawHtml && rvdomChildren.rawHtml === vdom.rawHtml) {
            // nothing to do
        }
        else {
            // render and reset the rvdom
            removeChildren(container, callbacksToRelease, rvdomChildren);
            var newRvdomChildren = renderChildren(container, vdom);
            rvdomChildren.rawHtml = newRvdomChildren.rawHtml;
            rvdomChildren.normal = newRvdomChildren.normal;
            rvdomChildren.keyed = newRvdomChildren.keyed;
        }
    }
    // modifies rvdom in place, appends to callbacksToRelease
    function patchNode(rvdom, callbacksToRelease, vdom) {
        var prevVdom = rvdom.vdom;
        function incompatible() {
            // remove all the children and swap the topmost mode
            var container = rvdom.dom.parentNode;
            // it is important that we remove before we mount, otherwise
            // the willRemove will be called before the didMount, which
            // is bizarre and less useful -- for example it prevents us
            // from implementing components. to do that, we first save the
            // next node to know where to insert.
            var nextChild = rvdom.dom.nextSibling;
            removeNode(container, rvdom, callbacksToRelease);
            render(vdom, function (newRvdom) {
                rvdom.children = newRvdom.children;
                rvdom.defaultProperties = newRvdom.defaultProperties;
                rvdom.dom = newRvdom.dom;
                rvdom.vdom = newRvdom.vdom;
                container.insertBefore(rvdom.dom, nextChild);
            });
        }
        function patch() {
            if (prevVdom.element && vdom.element && prevVdom.element.tag === vdom.element.tag) {
                var domEl = rvdom.dom;
                var prevVdomEl = prevVdom.element;
                var vdomEl = vdom.element;
                // will patch callback
                VDom.callLifecycleCallback(domEl, prevVdom.callbacks, "willPatch");
                // set all there is to set
                addProperties(rvdom.defaultProperties, rvdom.dom, vdomEl.properties, prevVdomEl.properties);
                addAttributes(domEl, vdomEl.attributes, prevVdomEl.attributes);
                addStyle(domEl, vdomEl.style, prevVdomEl.style);
                addClasses(domEl, vdomEl.classes, prevVdomEl.classes);
                addEvents(domEl, vdomEl.events, prevVdomEl.events);
                // release events from previous node
                for (var _i = 0, _a = prevVdomEl.events; _i < _a.length; _i++) {
                    var event_5 = _a[_i];
                    callbacksToRelease.push(event_5.token);
                }
                // recurse down
                patchChildren(domEl, rvdom.children, callbacksToRelease, vdomEl.children);
                // did patch callback for _new_ node
                VDom.callLifecycleCallback(domEl, vdom.callbacks, "didPatch");
            }
            else if (prevVdom.text && vdom.text && prevVdom.text === vdom.text) {
                // we're already done
            }
            else {
                incompatible();
            }
        }
        // if the nodes are marked, patch only if they're equal -- otherwise
        // restart. this is mostly for perf. reasons and in line with react's
        // components
        if (prevVdom.mark && vdom.mark && prevVdom.mark === vdom.mark) {
            patch();
        }
        else if (prevVdom.mark || vdom.mark) {
            incompatible();
        }
        else {
            patch();
        }
        // release callbacks from previous node
        releaseLifecycleCallbacks(callbacksToRelease, prevVdom.callbacks);
        rvdom.vdom = vdom;
    }
    function followPath(rendered, path, cont) {
        var root = rendered;
        for (var i = 0; i < path.length; i++) {
            var segment = path[i];
            var children = root.children;
            if (children.normal) {
                if (typeof segment === "number") {
                    root = children.normal[segment];
                }
                else {
                    throw "Got string segment for normal children";
                }
            }
            else if (children.keyed) {
                if (typeof segment === "string") {
                    root = children.keyed.elements[segment];
                }
                else {
                    throw "Got number segment for keyed children";
                }
            }
            else if (children.rawHtml) {
                throw "Got raw html even if I had some segments left";
            }
            else {
                throw "Got no normal or keyed children";
            }
        }
        return cont(root);
    }
    function reconciliate(rvdomRoot, path, vdom) {
        var callbacksToRelease = [];
        // find the position where we need to patch and patch
        followPath(rvdomRoot, path, function (rvdom) { return patchNode(rvdom, callbacksToRelease, vdom); });
        return callbacksToRelease;
    }
    Render.reconciliate = reconciliate;
})(Render || (Render = {}));
// use brackets to trick closure to not optimize this
window["Anapo"] = window["Anapo"] || {};
window["Anapo"]["render"] = Render.render;
window["Anapo"]["reconciliate"] = Render.reconciliate;
window["Anapo"]["newElement"] = VDom.newElement;
window["Anapo"]["newKeyedChildren"] = VDom.newKeyedChildren;
window["Anapo"]["newEventCallback"] = VDom.newEventCallback;
