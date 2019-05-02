type SimpleMap<V> = {[key: string]: V};

interface OrderedMap<V> {
  order: string[],
  elements: SimpleMap<V>,
}

type SimpleSet = SimpleMap<boolean>;

namespace VDom {
  // ------------------------------------------------------------------
  // types

  export type Mark = string;

  export interface VDomNode {
    // mark
    mark?: Mark,
    // body -- one of the ones below
    element?: VDomElement,
    text?: string,
    raw?: Node,
    // callbacks
    callbacks?: LifecycleCallbacks,
  }

  export type LifecycleCallbackName = "willMount" | "didMount" | "willPatch" | "didPatch" | "willRemove";
  export type LifecycleCallbacks = {[K in LifecycleCallbackName]?: LifecycleCallback};

  export type CallbackToken = any;

  export interface LifecycleCallback {
    callback(el: Node): void;
    token: CallbackToken,
  }

  export type Properties = {[property: string]: any}

  export type Style = {[style: string]: string};

  export type Attributes = {[attribute: string]: any};

  export type Classes = SimpleSet;

  export interface VDomElement {
    tag: string,
    properties: Properties,
    style: Style,
    attributes: Attributes,
    classes: Classes,
    events: EventCallback[],
    children: Children<VDomNode>,
  }

  export interface EventCallback {
    type: string,
    callback(this: HTMLElement, ev: Event): void,
    token: CallbackToken,
  }

  export interface Children<T> {
    rawHtml?: string,
    normal?: T[],
    keyed?: OrderedMap<T>,
  }

  // utils
  // ------------------------------------------------------------------

  export function callLifecycleCallback(dom: Node, callbacks: VDom.LifecycleCallbacks | undefined, callback: VDom.LifecycleCallbackName) {
    if (callbacks && callbacks[callback]) {
      callbacks[callback]!.callback(dom); // why do we need ! here?
    }
  }

  // we need these because we cannot easily construct object literals
  // from haskell, because the closure compiler is prone to renaming
  // their fields
  export function newElement(tag: string): VDomElement {
    return {tag: tag, style: {}, properties: {}, attributes: {}, classes: {}, events: [], children: {}};
  }
  export function newKeyedChildren(): OrderedMap<VDomNode> {
    return {order: [], elements: {}};
  }
  export function newEventCallback(type_: string, callback: (this: HTMLElement, ev: Event) => void, token: any): EventCallback {
    return {type: type_, callback: callback, token: token};
  }
  export function newLifecycleCallback(callback: (el: Node) => void, token: any): LifecycleCallback {
    return {callback: callback, token: token};
  }
}

namespace Render {
  type DefaultProperties = { [property: string]: any };

  interface EventListener {
    type: string;
    listener: (this: HTMLElement, event: Event) => void;
    token: VDom.CallbackToken;
  }

  export interface RenderedNode {
    vdom: VDom.VDomNode,
    dom: Node,
    // stores the default properties for the set properties. if the element has
    // properties, the keys here _must be the same_ as the keys in the element
    // properties. only present if the node is an element
    defaultProperties?: DefaultProperties,
    // finally the children
    children?: VDom.Children<RenderedNode>,
  }

  function stringEq(prop1: any, prop2: any) {
    if (typeof prop1 === "string" && typeof prop2 === "string") {
      return (<string>prop1) === (<string>prop2);
    }
    return false;
  }

  // modifies the defProps in place.
  function addProperties(defProps: DefaultProperties, el: Node, props: VDom.Properties, prevProps: VDom.Properties) {
    // remove properties that are not there anymore
    for (const prop in prevProps) {
      if (props[prop] === undefined) {
        (<any>el)[prop] = defProps[prop];
        delete defProps[prop];
      }
    }
    // add the new ones
    for (const prop in props) {
      if (prevProps[prop] !== undefined) {
        defProps[prop] = defProps[prop];
        // if the previous properties had the prop already, set the property unless
        // they're the same
        if (!stringEq(props[prop], prevProps[prop])) {
          (<any>el)[prop] = props[prop];
        }
      } else {
        // if it was not in the previous one, store the default property
        const defProp = (<any>el)[prop];
        defProps[prop] = defProp;
        (<any>el)[prop] = props[prop];
      }
    }
  }

  function addAttributes(dom: HTMLElement, attrs: VDom.Attributes, prevAttrs: VDom.Attributes) {
    // remove all the attrs that are not there anymore
    for (const attr in prevAttrs) {
      if (attrs[attr] === undefined) {
        dom.removeAttribute(attr);
      }
    }
    // add the others
    for (const attr in attrs) {
      // only set if they're not equal
      if (prevAttrs[attr] === undefined || !stringEq(prevAttrs[attr], attrs[attr])) {
        dom.setAttribute(attr, attrs[attr]);
      }
    }
  }

  function addStyle(dom: HTMLElement, style: VDom.Style, prevStyle: VDom.Style) {
    const css = dom.style;
    // remove all the keys that are not there anymore
    for (const k in prevStyle) {
      if (style[k] === undefined) {
        css.removeProperty(k);
      }
    }
    // add all the new ones
    for (const k in style) {
      // only set if they're not equal
      if (prevStyle[k] === undefined || style[k] !== prevStyle[k]) {
        css.setProperty(k, style[k]);
      }
    }
  }

  function addClasses(dom: HTMLElement, classes: VDom.Classes, prevClasses: VDom.Classes) {
    // note that we allow people to put spaces in classes.
    const domClasses = dom.classList;
    // remove all the classes that are not there anymore
    for (const cls in prevClasses) {
      if (classes[cls] === undefined) {
        domClasses.remove(cls);
      }
    }
    // add the new ones
    for (const cls in classes) {
      if (prevClasses[cls] === undefined) {
        domClasses.add(cls);
      }
    }
  }

  // appends to the tokens
  function addEvents(dom: HTMLElement, events: VDom.EventCallback[], oldEvents: VDom.EventCallback[]) {
    // remove the old ones
    for (const event of oldEvents) {
      dom.removeEventListener(event.type, event.callback);
    }
    // add the new ones
    for (const event of events) {
      dom.addEventListener(event.type, event.callback);
    }
  }

  function renderDom(container: HTMLElement, children: VDom.VDomNode[]): RenderedNode[] {
    return children.map((child) => render(child, (rvdom) => {
      container.appendChild(rvdom.dom)
      return rvdom;
    }));
  }

  function renderKeyedDom(container: HTMLElement, children: OrderedMap<VDom.VDomNode>): OrderedMap<RenderedNode> {
    const elements: SimpleMap<RenderedNode> = {};
    for (const key of children.order) {
      const child = children.elements[key];
      render(child, (rvdom) => {
        container.appendChild(rvdom.dom);
        elements[key] = rvdom;
      });
    }
    return {
      order: children.order,
      elements: elements,
    }
  }

  function renderChildren(container: HTMLElement, children: VDom.Children<VDom.VDomNode>): VDom.Children<RenderedNode> {
    if (children.normal) {
      return {normal: renderDom(container, children.normal)};
    }
    if (children.keyed) {
      return {keyed: renderKeyedDom(container, children.keyed)};
    }
    if (children.rawHtml !== undefined) {
      container.innerHTML = children.rawHtml;
      return {rawHtml: children.rawHtml};
    }
    throw "Couldn't find normal, keyed, or rawHtml";
  }

  export function render(vdom: VDom.VDomNode, cont: (rvdom: RenderedNode) => void): RenderedNode {
    function finalize(rvdom: RenderedNode) {
      VDom.callLifecycleCallback(rvdom.dom, vdom.callbacks, "willMount");
      cont(rvdom);
      VDom.callLifecycleCallback(rvdom.dom, vdom.callbacks, "didMount");
      return rvdom;
    }
    if (vdom.element) {
      const dom = document.createElement(vdom.element.tag);
      const rvdom: RenderedNode = {vdom, dom};
      rvdom.defaultProperties = {};
      addProperties(rvdom.defaultProperties, dom, vdom.element.properties, {});
      addAttributes(dom, vdom.element.attributes, {});
      addStyle(dom, vdom.element.style, {});
      addClasses(dom, vdom.element.classes, {});
      addEvents(dom, vdom.element.events, []);
      rvdom.children = renderChildren(dom, vdom.element.children);
      return finalize(rvdom);
    }
    if (vdom.text !== undefined) {
      return finalize({vdom, dom: document.createTextNode(vdom.text)});
    }
    if (vdom.raw !== undefined) {
      return finalize({vdom, dom: vdom.raw});
    }
    throw "VDOM didn't contain element, text, or raw element.";
  }

  export type VDomPathSegment = number | string;
  export type VDomPath = VDomPathSegment[];

  function releaseLifecycleCallback(callbacksToRelease: VDom.CallbackToken[], callbacks: VDom.LifecycleCallbacks | undefined, callback: VDom.LifecycleCallbackName) {
    if (callbacks && callbacks[callback]) {
      callbacksToRelease.push(callback)
    }
  }

  function releaseLifecycleCallbacks(callbacksToRelease: VDom.CallbackToken[], callbacks?: VDom.LifecycleCallbacks) {
    releaseLifecycleCallback(callbacksToRelease, callbacks, "willMount");
    releaseLifecycleCallback(callbacksToRelease, callbacks, "didMount");
    releaseLifecycleCallback(callbacksToRelease, callbacks, "willPatch");
    releaseLifecycleCallback(callbacksToRelease, callbacks, "didPatch")
    releaseLifecycleCallback(callbacksToRelease, callbacks, "willRemove");
  }

  // appends to callbacksToRelease
  function cleanupNode(rvdom: RenderedNode, callbacksToRelease: VDom.CallbackToken[]) {
    // call the callback
    VDom.callLifecycleCallback(rvdom.dom, rvdom.vdom.callbacks, "willRemove");
    // recurse down
    if (rvdom.children) {
      cleanupChildren(rvdom.children, callbacksToRelease);
    }
    // put the tokens for the callbacks and event listeners
    releaseLifecycleCallbacks(callbacksToRelease, rvdom.vdom.callbacks);
    if (rvdom.vdom.element) {
      for (const event of rvdom.vdom.element.events) {
        callbacksToRelease.push(event.token);
      }
    }
  }

  // appends to callbacksToRelease
  function cleanupChildren(rchildren: VDom.Children<RenderedNode>, callbacksToRelease: VDom.CallbackToken[]) {
    if (rchildren.rawHtml !== undefined) {
      // nothing to do
    } else if (rchildren.normal) {
      rchildren.normal.forEach((rvdom) => cleanupNode(rvdom, callbacksToRelease));
    } else if (rchildren.keyed) {
      for (const k of rchildren.keyed.order) {
        cleanupNode(rchildren.keyed.elements[k], callbacksToRelease);
      }
    } else {
      throw "Could not find normal, keyed, or raw html";
    }
  }

  // appends to callbacksToRelease
  function removeNode(container: HTMLElement, rvdom: RenderedNode, callbacksToRelease: VDom.CallbackToken) {
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
      for (const event of rvdom.vdom.element.events) {
        callbacksToRelease.push(event.token);
      }
    }
  }

  // removes all elements of rvdomChildren past the cursor
  function removeDom(domEl: HTMLElement, rvdomChildren: RenderedNode[], callbacksToRelease: VDom.CallbackToken[], cursor: number) {
    for (let i = cursor; i < rvdomChildren.length; i++) {
      const rvdom = rvdomChildren[i];
      removeNode(domEl, rvdomChildren[i], callbacksToRelease);
    }
    rvdomChildren.length = cursor;
  }

  // modifies container and rvdomChildren, appends to callbacksToRelease
  function patchDom(container: HTMLElement, rvdomChildren: RenderedNode[], callbacksToRelease: VDom.CallbackToken[], vdomChildren: VDom.VDomNode[]) {
    // first patch the common ones
    for (let i = 0; i < Math.min(rvdomChildren.length, vdomChildren.length); i++) {
      patchNode(rvdomChildren[i], callbacksToRelease, vdomChildren[i]);
    }
    // then, if we have more new ones, render them and push them
    if (vdomChildren.length > rvdomChildren.length) {
      rvdomChildren.push(...renderDom(container, vdomChildren.slice(rvdomChildren.length)));
    }
    // on the other hand if we have leftovers, remove them
    if (rvdomChildren.length > vdomChildren.length) {
      removeDom(container, rvdomChildren, callbacksToRelease, vdomChildren.length);
    }
  }

  // if the cursor is null, the node will be inserted at the beginning.
  function appendAfter(container: Node, cursor: Node | null, node: Node) {
    if (cursor) {
      // if the cursor has no next sibling it'll append it at the end, which is what
      // we want.
      container.insertBefore(node, cursor.nextSibling);
    } else {
      // if the cursor is null, put it before the first element. note that
      // if there is no first element it'll just append the node, which is fine
      // since it's the only node.
      container.insertBefore(node, container.firstChild);
    }
  }

  // TODO this is a pretty dumb implementation, use a more principled one (in terms of performance)
  function patchKeyedDom(container: HTMLElement, rvdomChildren: OrderedMap<RenderedNode>, callbacksToRelease: VDom.CallbackToken[], vdomChildren: OrderedMap<VDom.VDomNode>) {
    // assumes that there are no duplicates in both .orders.

    // * traverse the two .orders at the same time
    // * if you meet the same key, patch the node
    // * if you meet different keys:
    //   - if you can get the key from the rvdom, do it and patch the node.
    //   - if you can't, render the node and add it to the rvdom.
    //   in both cases, store the current rvdom key as skipped.
    // * when one of the two orders has finished

    const patchedChildren: {[key: string]: boolean} = {}; // these are the children from later in rvdomOrder that we already patched.

    function tryExistingChild(mbCursor: Node | null, vdomKey: string): Node { // returns the new cursor
      // if the key did change, see if we can find the new key in the rvdom
      const rvdomNode = rvdomChildren.elements[vdomKey];
      if (rvdomNode) {
        // if we can, move its dom after the previous element, and the patch it
        appendAfter(container, mbCursor, rvdomNode.dom);
        patchNode(rvdomNode, callbacksToRelease, vdomChildren.elements[vdomKey]);
        patchedChildren[vdomKey] = true;
        return rvdomNode.dom;
      } else {
        // otherwise just render it
        const rvdom = render(vdomChildren.elements[vdomKey], (rvdom) => {
          appendAfter(container, mbCursor, rvdom.dom);
          rvdomChildren.elements[vdomKey] = rvdom;
        });
        return rvdom.dom;
      }
    }

    const rvdomOrder = rvdomChildren.order;
    const vdomOrder = vdomChildren.order;
    let mbCursor: Node | null = null; // a cursor to keep track of where we are in the dom.
    let rvdom_i = 0;
    let vdom_i = 0;

    for (; vdom_i < vdomOrder.length && rvdom_i < rvdomOrder.length;) {
      const rvdomKey = rvdomOrder[rvdom_i];
      // if we have already used the rvdomKey, skip
      if (patchedChildren[rvdomKey]) {
        rvdom_i++;
        continue;
      }
      const vdomKey = vdomOrder[vdom_i];
      // common case: the vdom didn't change
      if (rvdomKey === vdomKey) {
        const rvdomChild = rvdomChildren.elements[rvdomKey];
        patchNode(rvdomChild, callbacksToRelease, vdomChildren.elements[rvdomKey]);
        mbCursor = rvdomChild.dom;
        rvdom_i++;
        vdom_i++;
      } else if (vdomChildren.elements[rvdomKey] === undefined) {
        // if the old key does not exist at all in the old stuff, remove the current node.
        // this is to avoid shuffling around nodes when it's not needed
        removeNode(container, rvdomChildren.elements[rvdomKey], callbacksToRelease);
        rvdom_i++;  
      } else {
        mbCursor = tryExistingChild(mbCursor, vdomKey);
        vdom_i++;
      }
    }
    // render remaining nodes
    for (; vdom_i < vdomOrder.length; vdom_i++) {
      const vdomKey = rvdomOrder[vdom_i];
      mbCursor = tryExistingChild(mbCursor, vdomOrder[vdom_i]);
    }
    // Remove the leftover nodes
    for (; rvdom_i < rvdomOrder.length; rvdom_i++) {
      const rvdomKey = rvdomChildren.order[rvdom_i];
      if (patchedChildren[rvdomKey] === undefined) {
        removeNode(container, rvdomChildren.elements[rvdomKey], callbacksToRelease);
        delete rvdomChildren.elements[rvdomKey];
      }
    }
    // set the new order
    rvdomChildren.order = vdomOrder;
  }

  function removeChildren(container: HTMLElement, callbacksToRelease: VDom.CallbackToken[], rvdomChildren: VDom.Children<RenderedNode>) {
    if (rvdomChildren.rawHtml !== undefined) {
      container.innerHTML = ""
    } else if (rvdomChildren.normal) {
      removeDom(container, rvdomChildren.normal, callbacksToRelease, 0);
    } else if (rvdomChildren.keyed) {
      for (const key of rvdomChildren.keyed.order) {
        removeNode(container, rvdomChildren.keyed.elements[key], callbacksToRelease);
      }
      rvdomChildren.keyed.order = [];
      rvdomChildren.keyed.elements = {};
    } else {
      throw "Could not find raw html, normal, or keyed";
    }
  }

  // modifies domEl and rvdomChildren, appends to callbacksToRelease
  function patchChildren(container: HTMLElement, rvdomChildren: VDom.Children<RenderedNode>, callbacksToRelease: VDom.CallbackToken[], vdom: VDom.Children<VDom.VDomNode>) {
    if (rvdomChildren.normal && vdom.normal) {
      patchDom(container, rvdomChildren.normal, callbacksToRelease, vdom.normal);
    } else if (rvdomChildren.keyed && vdom.keyed) {
      patchKeyedDom(container, rvdomChildren.keyed, callbacksToRelease, vdom.keyed);
    } else if (rvdomChildren.rawHtml !== undefined && vdom.rawHtml !== undefined && rvdomChildren.rawHtml === vdom.rawHtml) {
      // nothing to do
    } else {
      // render and reset the rvdom
      removeChildren(container, callbacksToRelease, rvdomChildren);
      const newRvdomChildren = renderChildren(container, vdom);
      rvdomChildren.rawHtml = newRvdomChildren.rawHtml;
      rvdomChildren.normal = newRvdomChildren.normal;
      rvdomChildren.keyed = newRvdomChildren.keyed;
    }
  }

  // modifies rvdom in place, appends to callbacksToRelease
  function patchNode(rvdom: RenderedNode, callbacksToRelease: VDom.CallbackToken[], vdom: VDom.VDomNode) {
    const prevVdom: VDom.VDomNode = rvdom.vdom;
    function incompatible() {
      // remove all the children and swap the topmost mode
      const container = <HTMLElement>rvdom.dom.parentNode!;
      // it is important that we remove before we mount, otherwise
      // the willRemove will be called before the didMount, which
      // is bizarre and less useful -- for example it prevents us
      // from implementing components. to do that, we first save the
      // next node to know where to insert.
      const nextChild = rvdom.dom.nextSibling;
      removeNode(container, rvdom, callbacksToRelease);
      render(vdom, (newRvdom) => {
        rvdom.children = newRvdom.children;
        rvdom.defaultProperties = newRvdom.defaultProperties;
        rvdom.dom = newRvdom.dom;
        rvdom.vdom = newRvdom.vdom;
        container.insertBefore(rvdom.dom, nextChild);
      });
    }
    function patch() {
      if (prevVdom.element && vdom.element && prevVdom.element.tag === vdom.element.tag) {
        const domEl = <HTMLElement>rvdom.dom;
        const prevVdomEl = prevVdom.element;
        const vdomEl = vdom.element;
        // will patch callback
        VDom.callLifecycleCallback(domEl, prevVdom.callbacks, "willPatch");
        // set all there is to set
        addProperties(rvdom.defaultProperties!, rvdom.dom, vdomEl.properties, prevVdomEl.properties);
        addAttributes(domEl, vdomEl.attributes, prevVdomEl.attributes);
        addStyle(domEl, vdomEl.style, prevVdomEl.style);
        addClasses(domEl, vdomEl.classes, prevVdomEl.classes);
        addEvents(domEl, vdomEl.events, prevVdomEl.events);
        // release events from previous node
        for (const event of prevVdomEl.events) {
          callbacksToRelease.push(event.token);
        }
        // recurse down
        patchChildren(domEl, rvdom.children!, callbacksToRelease, vdomEl.children);
        // did patch callback for _new_ node
        VDom.callLifecycleCallback(domEl, vdom.callbacks, "didPatch");
      } else if (prevVdom.text !== undefined && vdom.text !== undefined && prevVdom.text === vdom.text) {
        // we're already done
      } else {
        incompatible();
      }
    }
    // if the nodes are marked, patch only if they're equal -- otherwise
    // restart. this is mostly for perf. reasons and in line with react's
    // components
    if (prevVdom.mark && vdom.mark && prevVdom.mark === vdom.mark) {
      patch();
    } else if (prevVdom.mark || vdom.mark) {
      incompatible();
    } else {
      patch();
    }
    // release callbacks from previous node
    releaseLifecycleCallbacks(callbacksToRelease, prevVdom.callbacks);
    rvdom.vdom = vdom;
  }

  function followPath<T>(rendered: RenderedNode, path: VDomPath, cont: (rvdom: RenderedNode) => T): T {
    let root = rendered;
    for (let i = 0; i < path.length; i++) {
      const segment = path[i];
      const children = root.children!;
      if (children.normal) {
        if (typeof segment === "number") {
          root = children.normal[segment];
        } else {
          throw "Got string segment for normal children";
        }
      } else if (children.keyed) {
        if (typeof segment === "string") {
          root = children.keyed.elements[segment];
        } else {
          throw "Got number segment for keyed children";
        }
      } else if (children.rawHtml !== undefined) {
        throw "Got raw html even if I had some segments left";
      } else {
        throw "Got no normal or keyed children";
      }
    }
    return cont(root);
  }

  export function reconciliate(rvdomRoot: RenderedNode, path: VDomPath, vdom: VDom.VDomNode): VDom.CallbackToken[] {
    const callbacksToRelease: VDom.CallbackToken[] = []
    // find the position where we need to patch and patch
    followPath(rvdomRoot, path, (rvdom) => patchNode(rvdom, callbacksToRelease, vdom));
    return callbacksToRelease;
  }
}

// use brackets to trick closure to not optimize this
(<any>window)["Anapo"] = (<any>window)["Anapo"] || {};
(<any>window)["Anapo"]["render"] = Render.render;
(<any>window)["Anapo"]["reconciliate"] = Render.reconciliate;
(<any>window)["Anapo"]["newElement"] = VDom.newElement;
(<any>window)["Anapo"]["newKeyedChildren"] = VDom.newKeyedChildren;
(<any>window)["Anapo"]["newEventCallback"] = VDom.newEventCallback;
(<any>window)["Anapo"]["newLifecycleCallback"] = VDom.newLifecycleCallback;
