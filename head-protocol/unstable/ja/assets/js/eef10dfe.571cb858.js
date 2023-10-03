"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[7174],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>m});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function p(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),l=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=l(e.components);return a.createElement(s.Provider,{value:t},e.children)},c="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},h=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,u=p(e,["components","mdxType","originalType","parentName"]),c=l(n),h=r,m=c["".concat(s,".").concat(h)]||c[h]||d[h]||o;return n?a.createElement(m,i(i({ref:t},u),{},{components:n})):a.createElement(m,i({ref:t},u))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=h;var p={};for(var s in t)hasOwnProperty.call(t,s)&&(p[s]=t[s]);p.originalType=e,p[c]="string"==typeof e?e:r,i[1]=p;for(var l=2;l<o;l++)i[l]=n[l];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}h.displayName="MDXCreateElement"},69376:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>d,frontMatter:()=>o,metadata:()=>p,toc:()=>l});var a=n(87462),r=(n(67294),n(3905));const o={sidebar_position:4},i="API Behavior",p={unversionedId:"behavior",id:"behavior",title:"API Behavior",description:"This page documents the behavior of a hydra-node at the API layer. That is, how the system behaves given ClientInputs and what ServerOutputs are produced in response to it. See also the API reference for more details about individual API messages. The only discrepancy is http POST /commit action which is not a state transition but a user action that submits a commit transaction which should produce Committed output.",source:"@site/core-concepts/behavior.md",sourceDirName:".",slug:"/behavior",permalink:"/head-protocol/unstable/ja/core-concepts/behavior",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/core-concepts/behavior.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"defaultSidebar",previous:{title:"Hydra Networking",permalink:"/head-protocol/unstable/ja/core-concepts/architecture/networking"},next:{title:"\u30ed\u30fc\u30eb\u30d0\u30c3\u30af\u51e6\u7406",permalink:"/head-protocol/unstable/ja/core-concepts/rollbacks/"}},s={},l=[{value:"API configuration",id:"api-configuration",level:4},{value:"Replay of past server outputs",id:"replay-of-past-server-outputs",level:2}],u={toc:l},c="wrapper";function d(e){let{components:t,...n}=e;return(0,r.kt)(c,(0,a.Z)({},u,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"api-behavior"},"API Behavior"),(0,r.kt)("p",null,"This page documents the behavior of a ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," at the API layer. That is, how the system behaves given ",(0,r.kt)("a",{parentName:"p",href:"pathname:///haddock/hydra-node/Hydra-API-ClientInput.html#t:ClientInput"},"ClientInputs")," and what ",(0,r.kt)("a",{parentName:"p",href:"pathname:///haddock/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput"},"ServerOutputs")," are produced in response to it. See also the ",(0,r.kt)("a",{parentName:"p",href:"/api-reference/"},"API reference")," for more details about individual API messages. The only discrepancy is http ",(0,r.kt)("inlineCode",{parentName:"p"},"POST /commit")," action which is not a state transition but a user action that submits a commit transaction which should produce ",(0,r.kt)("inlineCode",{parentName:"p"},"Committed")," output."),(0,r.kt)("p",null,"The formalism uses ",(0,r.kt)("a",{parentName:"p",href:"https://en.wikipedia.org/wiki/UML_state_machine"},"UML statechart")," language where transitions are labeled: ",(0,r.kt)("inlineCode",{parentName:"p"},"input [condition] / output"),". When two outputs (e.g. ",(0,r.kt)("inlineCode",{parentName:"p"},"A")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"B"),") are expected we write ",(0,r.kt)("inlineCode",{parentName:"p"},"A,B"),", while ",(0,r.kt)("inlineCode",{parentName:"p"},"{A,B}")," denotes mutual exclusiveness of outputs."),(0,r.kt)("p",null,(0,r.kt)("img",{parentName:"p",src:"https://www.plantuml.com/plantuml/svg/ZP71JW8n48RlVOevge5mvs0mH2CN8RBnGZWKEiWaxNJJ3hfWV7VRBIw87hnrPhvl_-vq54K7sJchjcGGqDMo1uDn7QWMygpKucO9_VujJ9Y4jAK3yIiCsn86y8pQx2i_ziwHAFK3-YrTpQRp2WRhbhvEUl44pOMPr0TYRPDpj_8X9pscf4dCrP_uj4PEz3UNIwNQvcduXEzLav2Fgdb9hkbLpOJVZgVxfgQ0vhCtPrt7hPUnvmq5XwPy9eUChOzeO5WENLXfAtKSduCTubam2feEoh-esUzavcEabSL4BuGSGgrZn0Xw8nZ09DqIu_AqxA8fTQ7tBMxaR75btDsWRTCXtxCGXV_VmuwGpSxPBm00",alt:null})),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"https://www.plantuml.com/plantuml/uml/ZP71JW8n48RlVOevge5mvs0mH2CN8RBnGZWKEiWaxNJJ3hfWV7VRBIw87hnrPhvl_-vq54K7sJchjcGGqDMo1uDn7QWMygpKucO9_VujJ9Y4jAK3yIiCsn86y8pQx2i_ziwHAFK3-YrTpQRp2WRhbhvEUl44pOMPr0TYRPDpj_8X9pscf4dCrP_uj4PEz3UNIwNQvcduXEzLav2Fgdb9hkbLpOJVZgVxfgQ0vhCtPrt7hPUnvmq5XwPy9eUChOzeO5WENLXfAtKSduCTubam2feEoh-esUzavcEabSL4BuGSGgrZn0Xw8nZ09DqIu_AqxA8fTQ7tBMxaR75btDsWRTCXtxCGXV_VmuwGpSxPBm00"},"Edit this diagram")),(0,r.kt)("p",null,"Not pictured is the ",(0,r.kt)("inlineCode",{parentName:"p"},"CommandFailed")," output, which is implicit emitted whenever an input is used when no transition below applies. Also non-state-changing or life-cycle relevant inputs like ",(0,r.kt)("inlineCode",{parentName:"p"},"GetUTxO")," are not mentioned, as well as outputs like ",(0,r.kt)("inlineCode",{parentName:"p"},"Greetings"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"InvalidInput"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"PeerConnected"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"PeerDisconnected")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"GetUTxOResponse"),"."),(0,r.kt)("h4",{id:"api-configuration"},"API configuration"),(0,r.kt)("p",null,"There are some options for API clients to control the server outputs. Server outputs are controlled using the following query parameters:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"history=no")," -> Prevents historical outputs display. All server outputs are recorded and when a client re-connects these outputs are replayed unless ",(0,r.kt)("inlineCode",{parentName:"li"},"history=no")," query param is used."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"tx-output=cbor")," -> Outputs transaction fields encoded as CBOR instead of default JSON."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"snapshot-utxo=no")," -> In case of a ",(0,r.kt)("inlineCode",{parentName:"li"},"SnapshotConfirmed")," message the ",(0,r.kt)("inlineCode",{parentName:"li"},"utxo")," field in the inner ",(0,r.kt)("inlineCode",{parentName:"li"},"Snapshot")," will be omitted.")),(0,r.kt)("h2",{id:"replay-of-past-server-outputs"},"Replay of past server outputs"),(0,r.kt)("p",null,"When a ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," restarts, by default it will load it's history from persistence and replay previous server outputs to enable clients to re-establish their state upon re-connection. If that happens, obviously some of these outputs are not relevant anymore. One example of this is the ",(0,r.kt)("inlineCode",{parentName:"p"},"PeerConnected")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"PeerDisconnected"),". To make it possible to determine the end of replayed history, client applications can use the ",(0,r.kt)("inlineCode",{parentName:"p"},"Greetings"),", which will be emitted on every ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," start. See the ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-tui")," example client for how this is handled."),(0,r.kt)("p",null,"Clients can optionally decide to skip history outputs and receive only the ",(0,r.kt)("inlineCode",{parentName:"p"},"Greetings")," and following ones. In order to do that they can use query param ",(0,r.kt)("inlineCode",{parentName:"p"},"history=no"),"."),(0,r.kt)("p",null,"For example if the client wants to connect to a local ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," and doesn't want to view the server history but also want to have the transactions encoded as CBOR (base16) and prevent utxo display in ",(0,r.kt)("inlineCode",{parentName:"p"},"SnapshotConfirmed")," messages, they would connect using default port ",(0,r.kt)("inlineCode",{parentName:"p"},"4001")," and the full path ",(0,r.kt)("inlineCode",{parentName:"p"},"ws://localhost:4001/?history=no&tx-output=cbor"),"."))}d.isMDXComponent=!0}}]);