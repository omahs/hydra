"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2627],{3905:(t,a,e)=>{e.d(a,{Zo:()=>d,kt:()=>h});var r=e(67294);function n(t,a,e){return a in t?Object.defineProperty(t,a,{value:e,enumerable:!0,configurable:!0,writable:!0}):t[a]=e,t}function i(t,a){var e=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);a&&(r=r.filter((function(a){return Object.getOwnPropertyDescriptor(t,a).enumerable}))),e.push.apply(e,r)}return e}function l(t){for(var a=1;a<arguments.length;a++){var e=null!=arguments[a]?arguments[a]:{};a%2?i(Object(e),!0).forEach((function(a){n(t,a,e[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(e)):i(Object(e)).forEach((function(a){Object.defineProperty(t,a,Object.getOwnPropertyDescriptor(e,a))}))}return t}function g(t,a){if(null==t)return{};var e,r,n=function(t,a){if(null==t)return{};var e,r,n={},i=Object.keys(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||(n[e]=t[e]);return n}(t,a);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||Object.prototype.propertyIsEnumerable.call(t,e)&&(n[e]=t[e])}return n}var m=r.createContext({}),p=function(t){var a=r.useContext(m),e=a;return t&&(e="function"==typeof t?t(a):l(l({},a),t)),e},d=function(t){var a=p(t.components);return r.createElement(m.Provider,{value:a},t.children)},k="mdxType",N={inlineCode:"code",wrapper:function(t){var a=t.children;return r.createElement(r.Fragment,{},a)}},o=r.forwardRef((function(t,a){var e=t.components,n=t.mdxType,i=t.originalType,m=t.parentName,d=g(t,["components","mdxType","originalType","parentName"]),k=p(e),o=n,h=k["".concat(m,".").concat(o)]||k[o]||N[o]||i;return e?r.createElement(h,l(l({ref:a},d),{},{components:e})):r.createElement(h,l({ref:a},d))}));function h(t,a){var e=arguments,n=a&&a.mdxType;if("string"==typeof t||n){var i=e.length,l=new Array(i);l[0]=o;var g={};for(var m in a)hasOwnProperty.call(a,m)&&(g[m]=a[m]);g.originalType=t,g[k]="string"==typeof t?t:n,l[1]=g;for(var p=2;p<i;p++)l[p]=e[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,e)}o.displayName="MDXCreateElement"},97642:(t,a,e)=>{e.r(a),e.d(a,{assets:()=>m,contentTitle:()=>l,default:()=>N,frontMatter:()=>i,metadata:()=>g,toc:()=>p});var r=e(87462),n=(e(67294),e(3905));const i={sidebar_label:"Transactions Costs",sidebar_position:3},l="Transactions Costs",g={unversionedId:"transaction-cost",id:"transaction-cost",title:"Transactions Costs",description:"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using arbitrary values and results are not fully deterministic and comparable to previous runs.",source:"@site/benchmarks/transaction-cost.md",sourceDirName:".",slug:"/transaction-cost",permalink:"/head-protocol/benchmarks/transaction-cost",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/benchmarks/transaction-cost.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_label:"Transactions Costs",sidebar_position:3},sidebar:"defaultSidebar",previous:{title:"Benchmarks",permalink:"/head-protocol/benchmarks/"},next:{title:"End-to-End Benchmarks",permalink:"/head-protocol/benchmarks/end-to-end-benchmarks"}},m={},p=[{value:"Script summary",id:"script-summary",level:2},{value:"Cost of Init Transaction",id:"cost-of-init-transaction",level:2},{value:"Cost of Commit Transaction",id:"cost-of-commit-transaction",level:2},{value:"Cost of CollectCom Transaction",id:"cost-of-collectcom-transaction",level:2},{value:"Cost of Close Transaction",id:"cost-of-close-transaction",level:2},{value:"Cost of Contest Transaction",id:"cost-of-contest-transaction",level:2},{value:"Cost of Abort Transaction",id:"cost-of-abort-transaction",level:2},{value:"Cost of FanOut Transaction",id:"cost-of-fanout-transaction",level:2}],d={toc:p},k="wrapper";function N(t){let{components:a,...e}=t;return(0,n.kt)(k,(0,r.Z)({},d,e,{components:a,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"transactions-costs"},"Transactions Costs"),(0,n.kt)("p",null,"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using ",(0,n.kt)("inlineCode",{parentName:"p"},"arbitrary")," values and results are not fully deterministic and comparable to previous runs."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Metadata"),(0,n.kt)("th",{parentName:"tr",align:"left"}))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Generated at")),(0,n.kt)("td",{parentName:"tr",align:"left"},"2023-08-25 14:17:06.563390092 UTC")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. memory units")),(0,n.kt)("td",{parentName:"tr",align:"left"},"14000000")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. CPU units")),(0,n.kt)("td",{parentName:"tr",align:"left"},"10000000000")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. tx size (kB)")),(0,n.kt)("td",{parentName:"tr",align:"left"},"16384")))),(0,n.kt)("h2",{id:"script-summary"},"Script summary"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Name"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Hash"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Size (Bytes)"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdInitial"),(0,n.kt)("td",{parentName:"tr",align:"left"},"3ffaf6b87df35cb01a52eb23032b8f0b1a2a3ad3acf0930abc9c833a"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4150")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdCommit"),(0,n.kt)("td",{parentName:"tr",align:"left"},"e4c32d6dc83b2917aa7805571f30437ad98b6d20d821d34d45943755"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2093")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdHead"),(0,n.kt)("td",{parentName:"tr",align:"left"},"8508839dfce39b6be65c018ce124ab549238e1b2ed08fb6588e5601f"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8799")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bcHead"),(0,n.kt)("td",{parentName:"tr",align:"left"},"7e939052f278f7721cb1f98c3f2ca646827df9314a7e0d0168765b49*"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4055")))),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per Head.")),(0,n.kt)("h2",{id:"cost-of-init-transaction"},"Cost of Init Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4652"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.17"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.43"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.48")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4854"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.17"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5.20"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.51")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5059"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.28"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.02"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.54")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5469"),(0,n.kt)("td",{parentName:"tr",align:"right"},"19.15"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.51"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.60")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6491"),(0,n.kt)("td",{parentName:"tr",align:"right"},"29.64"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.76")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"45"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13670"),(0,n.kt)("td",{parentName:"tr",align:"right"},"99.83"),(0,n.kt)("td",{parentName:"tr",align:"right"},"38.69"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.84")))),(0,n.kt)("h2",{id:"cost-of-commit-transaction"},"Cost of Commit Transaction"),(0,n.kt)("p",null," This is using ada-only outputs for better comparability."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"595"),(0,n.kt)("td",{parentName:"tr",align:"right"},"12.67"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.97"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.31")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"787"),(0,n.kt)("td",{parentName:"tr",align:"right"},"16.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.37")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"975"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.37"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.42"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.42")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1348"),(0,n.kt)("td",{parentName:"tr",align:"right"},"28.55"),(0,n.kt)("td",{parentName:"tr",align:"right"},"12.04"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.53")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2288"),(0,n.kt)("td",{parentName:"tr",align:"right"},"51.35"),(0,n.kt)("td",{parentName:"tr",align:"right"},"21.96"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.82")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"18"),(0,n.kt)("td",{parentName:"tr",align:"right"},"3781"),(0,n.kt)("td",{parentName:"tr",align:"right"},"95.26"),(0,n.kt)("td",{parentName:"tr",align:"right"},"40.52"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.38")))),(0,n.kt)("h2",{id:"cost-of-collectcom-transaction"},"Cost of CollectCom Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO (bytes)"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"813"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.69"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.45")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"left"},"114"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1136"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.76"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14.76"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.60")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"left"},"169"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1454"),(0,n.kt)("td",{parentName:"tr",align:"right"},"54.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"21.99"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.81")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"4"),(0,n.kt)("td",{parentName:"tr",align:"left"},"227"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1776"),(0,n.kt)("td",{parentName:"tr",align:"right"},"73.97"),(0,n.kt)("td",{parentName:"tr",align:"right"},"29.89"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.04")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"281"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2094"),(0,n.kt)("td",{parentName:"tr",align:"right"},"94.76"),(0,n.kt)("td",{parentName:"tr",align:"right"},"38.46"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.29")))),(0,n.kt)("h2",{id:"cost-of-close-transaction"},"Cost of Close Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"694"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.67"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.24"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.38")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"913"),(0,n.kt)("td",{parentName:"tr",align:"right"},"19.45"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.84"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.42")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1152"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.72"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.26"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.45")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1553"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.32"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14.43"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.52")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2632"),(0,n.kt)("td",{parentName:"tr",align:"right"},"32.73"),(0,n.kt)("td",{parentName:"tr",align:"right"},"22.16"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.69")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10645"),(0,n.kt)("td",{parentName:"tr",align:"right"},"93.19"),(0,n.kt)("td",{parentName:"tr",align:"right"},"79.73"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.95")))),(0,n.kt)("h2",{id:"cost-of-contest-transaction"},"Cost of Contest Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"722"),(0,n.kt)("td",{parentName:"tr",align:"right"},"23.02"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.27"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.44")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"940"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.98"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.93"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.48")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1111"),(0,n.kt)("td",{parentName:"tr",align:"right"},"25.95"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.04"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.50")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1580"),(0,n.kt)("td",{parentName:"tr",align:"right"},"29.96"),(0,n.kt)("td",{parentName:"tr",align:"right"},"16.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.58")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2614"),(0,n.kt)("td",{parentName:"tr",align:"right"},"39.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.37"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.76")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"44"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9412"),(0,n.kt)("td",{parentName:"tr",align:"right"},"99.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"76.24"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.92")))),(0,n.kt)("h2",{id:"cost-of-abort-transaction"},"Cost of Abort Transaction"),(0,n.kt)("p",null,"Some variation because of random mixture of still initial and already committed outputs."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4867"),(0,n.kt)("td",{parentName:"tr",align:"right"},"21.62"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.29"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.61")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5353"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.46"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.87"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.79")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5775"),(0,n.kt)("td",{parentName:"tr",align:"right"},"55.02"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.06"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.02")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"4"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6206"),(0,n.kt)("td",{parentName:"tr",align:"right"},"76.30"),(0,n.kt)("td",{parentName:"tr",align:"right"},"33.45"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.28")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6092"),(0,n.kt)("td",{parentName:"tr",align:"right"},"86.74"),(0,n.kt)("td",{parentName:"tr",align:"right"},"37.59"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.39")))),(0,n.kt)("h2",{id:"cost-of-fanout-transaction"},"Cost of FanOut Transaction"),(0,n.kt)("p",null,"Involves spending head output and burning head tokens. Uses ada-only UTxO for better comparability."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO (bytes)"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"0"),(0,n.kt)("td",{parentName:"tr",align:"left"},"0"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4676"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.05"),(0,n.kt)("td",{parentName:"tr",align:"right"},"3.80"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.46")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4708"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.50"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.47")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"284"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4850"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.61"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.80"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.55")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"left"},"570"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5028"),(0,n.kt)("td",{parentName:"tr",align:"right"},"21.91"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.68"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.63")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"20"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1140"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5387"),(0,n.kt)("td",{parentName:"tr",align:"right"},"35.29"),(0,n.kt)("td",{parentName:"tr",align:"right"},"19.78"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.82")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"30"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1705"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5749"),(0,n.kt)("td",{parentName:"tr",align:"right"},"48.26"),(0,n.kt)("td",{parentName:"tr",align:"right"},"27.71"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.99")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"40"),(0,n.kt)("td",{parentName:"tr",align:"left"},"2276"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6107"),(0,n.kt)("td",{parentName:"tr",align:"right"},"61.30"),(0,n.kt)("td",{parentName:"tr",align:"right"},"35.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.17")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"left"},"2845"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6468"),(0,n.kt)("td",{parentName:"tr",align:"right"},"74.70"),(0,n.kt)("td",{parentName:"tr",align:"right"},"43.77"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.35")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"69"),(0,n.kt)("td",{parentName:"tr",align:"left"},"3928"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7152"),(0,n.kt)("td",{parentName:"tr",align:"right"},"99.68"),(0,n.kt)("td",{parentName:"tr",align:"right"},"58.98"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.70")))))}N.isMDXComponent=!0}}]);