"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[204],{3905:(e,t,n)=>{n.d(t,{Zo:()=>m,kt:()=>k});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),d=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},m=function(e){var t=d(e.components);return a.createElement(s.Provider,{value:t},e.children)},p="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},u=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,m=i(e,["components","mdxType","originalType","parentName"]),p=d(n),u=r,k=p["".concat(s,".").concat(u)]||p[u]||c[u]||o;return n?a.createElement(k,l(l({ref:t},m),{},{components:n})):a.createElement(k,l({ref:t},m))}));function k(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,l=new Array(o);l[0]=u;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i[p]="string"==typeof e?e:r,l[1]=i;for(var d=2;d<o;d++)l[d]=n[d];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},8464:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>l,default:()=>c,frontMatter:()=>o,metadata:()=>i,toc:()=>d});var a=n(87462),r=(n(67294),n(3905));const o={sidebar_label:"End-to-End Benchmarks",sidebar_position:4},l="End-To-End Benchmark Results",i={unversionedId:"end-to-end-benchmarks",id:"end-to-end-benchmarks",title:"End-To-End Benchmark Results",description:"This page is intended to collect the latest end-to-end benchmarks  results produced by Hydra's Continuous Integration system from  the latest master code.",source:"@site/benchmarks/end-to-end-benchmarks.md",sourceDirName:".",slug:"/end-to-end-benchmarks",permalink:"/head-protocol/fr/benchmarks/end-to-end-benchmarks",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/benchmarks/end-to-end-benchmarks.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_label:"End-to-End Benchmarks",sidebar_position:4},sidebar:"defaultSidebar",previous:{title:"Transactions Costs",permalink:"/head-protocol/fr/benchmarks/transaction-cost"},next:{title:"Profiling Hydra scripts",permalink:"/head-protocol/fr/benchmarks/profiling"}},s={},d=[{value:"3-nodes Scenario",id:"3-nodes-scenario",level:2},{value:"Baseline Scenario",id:"baseline-scenario",level:2}],m={toc:d},p="wrapper";function c(e){let{components:t,...n}=e;return(0,r.kt)(p,(0,a.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"end-to-end-benchmark-results"},"End-To-End Benchmark Results"),(0,r.kt)("p",null,"This page is intended to collect the latest end-to-end benchmarks  results produced by Hydra's Continuous Integration system from  the latest ",(0,r.kt)("inlineCode",{parentName:"p"},"master")," code."),(0,r.kt)("admonition",{type:"caution"},(0,r.kt)("p",{parentName:"admonition"},"Please take those results with a grain of  salt as they are currently produced from very limited cloud VMs and not controlled  hardware. Instead of focusing on the ",(0,r.kt)("em",{parentName:"p"},"absolute")," results, the emphasis  should be on relative results, eg. how the timings for a scenario  evolve as the code changes.")),(0,r.kt)("p",null,(0,r.kt)("em",{parentName:"p"},"Generated at"),"  2023-08-25 14:11:16.118013268 UTC"),(0,r.kt)("h2",{id:"3-nodes-scenario"},"3-nodes Scenario"),(0,r.kt)("p",null,"A rather typical setup, with 3 nodes forming a Hydra head."),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Number of nodes"),(0,r.kt)("th",{parentName:"tr",align:null},"3"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Number of txs")),(0,r.kt)("td",{parentName:"tr",align:null},"900")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Avg. Confirmation Time (ms)")),(0,r.kt)("td",{parentName:"tr",align:null},"89.651055263")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"P99")),(0,r.kt)("td",{parentName:"tr",align:null},"241.93965799ms")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"P95")),(0,r.kt)("td",{parentName:"tr",align:null},"219.01675214999997ms")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"P50")),(0,r.kt)("td",{parentName:"tr",align:null},"60.377091ms")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Number of Invalid txs")),(0,r.kt)("td",{parentName:"tr",align:null},"0")))),(0,r.kt)("h2",{id:"baseline-scenario"},"Baseline Scenario"),(0,r.kt)("p",null,"This scenario represents a minimal case and as such is a good baseline against which to assess the overhead introduced by more complex setups. There is a single hydra-node d with a single client submitting single input and single output transactions with a  constant UTxO set of 1."),(0,r.kt)("table",null,(0,r.kt)("thead",{parentName:"table"},(0,r.kt)("tr",{parentName:"thead"},(0,r.kt)("th",{parentName:"tr",align:null},"Number of nodes"),(0,r.kt)("th",{parentName:"tr",align:null},"1"))),(0,r.kt)("tbody",{parentName:"table"},(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Number of txs")),(0,r.kt)("td",{parentName:"tr",align:null},"300")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Avg. Confirmation Time (ms)")),(0,r.kt)("td",{parentName:"tr",align:null},"3.932877666")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"P99")),(0,r.kt)("td",{parentName:"tr",align:null},"11.511496039999988ms")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"P95")),(0,r.kt)("td",{parentName:"tr",align:null},"8.183427950000002ms")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"P50")),(0,r.kt)("td",{parentName:"tr",align:null},"2.8971590000000003ms")),(0,r.kt)("tr",{parentName:"tbody"},(0,r.kt)("td",{parentName:"tr",align:null},(0,r.kt)("em",{parentName:"td"},"Number of Invalid txs")),(0,r.kt)("td",{parentName:"tr",align:null},"0")))))}c.isMDXComponent=!0}}]);