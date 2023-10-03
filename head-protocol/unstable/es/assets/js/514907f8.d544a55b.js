"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2171],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>c});var n=a(67294);function o(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function r(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?r(Object(a),!0).forEach((function(t){o(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):r(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,o=function(e,t){if(null==e)return{};var a,n,o={},r=Object.keys(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||(o[a]=e[a]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(o[a]=e[a])}return o}var l=n.createContext({}),p=function(e){var t=n.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},d=function(e){var t=p(e.components);return n.createElement(l.Provider,{value:t},e.children)},h="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,o=e.mdxType,r=e.originalType,l=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),h=p(a),m=o,c=h["".concat(l,".").concat(m)]||h[m]||u[m]||r;return a?n.createElement(c,i(i({ref:t},d),{},{components:a})):n.createElement(c,i({ref:t},d))}));function c(e,t){var a=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=a.length,i=new Array(r);i[0]=m;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[h]="string"==typeof e?e:o,i[1]=s;for(var p=2;p<r;p++)i[p]=a[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},5663:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>u,frontMatter:()=>r,metadata:()=>s,toc:()=>p});var n=a(87462),o=(a(67294),a(3905));const r={title:"November 2022",slug:"2022-11",authors:"ch1bo",tags:["monthly"]},i=void 0,s={permalink:"/head-protocol/unstable/es/monthly/2022-11",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/monthly/2022-11-monthly.md",source:"@site/monthly/2022-11-monthly.md",title:"November 2022",description:"Introduction",date:"2022-12-05T09:19:28.000Z",formattedDate:"5 de diciembre de 2022",tags:[{label:"monthly",permalink:"/head-protocol/unstable/es/monthly/tags/monthly"}],readingTime:8.5,hasTruncateMarker:!1,authors:[{name:"Sebastian Nagel",title:"Software Engineering Lead",url:"https://github.com/ch1bo",imageURL:"https://github.com/ch1bo.png",key:"ch1bo"}],frontMatter:{title:"November 2022",slug:"2022-11",authors:"ch1bo",tags:["monthly"]},prevItem:{title:"December 2022",permalink:"/head-protocol/unstable/es/monthly/2022-12"}},l={authorsImageUrls:[void 0]},p=[{value:"Introduction",id:"introduction",level:2},{value:"Roadmap update",id:"roadmap-update",level:2},{value:"Release <code>0.8.0</code>",id:"release-080",level:4},{value:"Release <code>0.8.1</code>",id:"release-081",level:4},{value:"Notable changes",id:"notable-changes",level:4},{value:"Development",id:"development",level:2},{value:"Formal verification &amp; specification",id:"formal-verification--specification",level:2},{value:"Product",id:"product",level:2},{value:"Team &amp; open source",id:"team--open-source",level:2},{value:"Cardano Summit &amp; workshop",id:"cardano-summit--workshop",level:2}],d={toc:p},h="wrapper";function u(e){let{components:t,...r}=e;return(0,o.kt)(h,(0,n.Z)({},d,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h2",{id:"introduction"},"Introduction"),(0,o.kt)("p",null,"This report summarizes the activities of the Hydra team since October 2022 and also serves as a preparation & write-up of the monthly review meeting, in which we update major stakeholders of the project on recent developments and gather their feedback on our proposed plan forward each month."),(0,o.kt)("h2",{id:"roadmap-update"},"Roadmap update"),(0,o.kt)("p",null,"Looking at our ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/orgs/input-output-hk/projects/21"},"roadmap")," we can report the following releases and updates:"),(0,o.kt)("h4",{id:"release-080"},"Release ",(0,o.kt)("inlineCode",{parentName:"h4"},"0.8.0")),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Long overdue feature release adding persistence to the hydra-node"),(0,o.kt)("li",{parentName:"ul"},"Backup & restore the state of a Hydra Head ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/issues/187"},"#187")),(0,o.kt)("li",{parentName:"ul"},"Improve user experience following hydraw experiment ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/issues/518"},"#518")," from being only a discussion to a (to-be-groomed & planned) feature"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/releases/tag/0.8.0"},"Full release notes"))),(0,o.kt)("h4",{id:"release-081"},"Release ",(0,o.kt)("inlineCode",{parentName:"h4"},"0.8.1")),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Follow-up release addressing user feedback on persistence from ",(0,o.kt)("inlineCode",{parentName:"li"},"0.8.0")),(0,o.kt)("li",{parentName:"ul"},"Allow clients to see latest state after restart ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/issues/580"},"#580")),(0,o.kt)("li",{parentName:"ul"},"Bug fixes of following chain state ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/issues/599"},"#599")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/releases/tag/0.8.1"},"Full release notes"))),(0,o.kt)("h4",{id:"notable-changes"},"Notable changes"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Impact mapping from workshop (see below) and reviewing the starmap above had us discuss whether the scope for ",(0,o.kt)("inlineCode",{parentName:"li"},"1.0.0")," is correct or not"),(0,o.kt)("li",{parentName:"ul"},"Need to be conscious of scope creep vs. getting something audited"),(0,o.kt)("li",{parentName:"ul"},"Focus on getting the spec done, ",(0,o.kt)("inlineCode",{parentName:"li"},"0.9.0")," tagged and an audit under way now \u2192 minor re-ordering in priorities"),(0,o.kt)("li",{parentName:"ul"},"Iterative releases afterwards still - even while getting audited, non-contract changes first"),(0,o.kt)("li",{parentName:"ul"},"Promoted ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/issues/635"},"https://github.com/input-output-hk/hydra/issues/635")," from being only a discussion to a (to-be-groomed & planned) feature")),(0,o.kt)("p",null,(0,o.kt)("img",{src:a(19405).Z,width:"4096",height:"1671"})),(0,o.kt)("small",null,(0,o.kt)("center",null,"Latest roadmap with 0.8.0 and 0.8.1 already released and slight re-ordering on 0.9.0")),(0,o.kt)("h2",{id:"development"},"Development"),(0,o.kt)("p",null,"Issues closed since last report: ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues?page=1&q=is%3Aclosed+sort%3Aupdated-desc+closed%3A%3E%3D2022-10-19"},"Issues - input-output-hk/hydra")),(0,o.kt)("p",null,"Besides the things in the releases above (see roadmap update), we have\nbeen working on:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Flaky TUI CI tests:")," We have sunk quite some time again in\ninvestigating cryptic CI failures in our TUI end-to-end tests. They\nhave been crashing abruptly without any information and thus\nhard-to-debug. This investigation was unsuccessful and we realized\nat some point that the TUI tests are not that important anyways - we\nhave API-level end-to-end tests. So we disabled these tests for now.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Published")," ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-cardano-api")," ",(0,o.kt)("strong",{parentName:"p"},"to CHaP:")," With the Cardano\nHaskell Packages (CHaP) becoming available now and wanting to be a\ngood citizen, we pushed for getting our flavor of the ",(0,o.kt)("inlineCode",{parentName:"p"},"cardano-api"),'\nalso published there. This is non-trivial though, as we are at an\nintegration point even "further up" than the ',(0,o.kt)("inlineCode",{parentName:"p"},"cardano-node"),"\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/504"},"#504"))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Implemented ADR18:")," While the first stints on persistence already\nmake it possible to restart ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," without needing to close a\nHead, we have implemented this in a nicer way now only keeping a\nsingle state (ground truth) for both L2 and L1 information\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/541"},"#541")," of the Head protocol and the specification, we kept\ndiscovering bigger and bigger issues and hence realized the need for\na discussion on transaction validity in context of closing /\ncontesting Heads. ",(0,o.kt)("a",{parentName:"p",href:"#615"},"#615")))),(0,o.kt)("h2",{id:"formal-verification--specification"},"Formal verification & specification"),(0,o.kt)("p",null,"An important part of the project right now is the formalization and\naudit preparation:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Internal audit")," has started with involvement of two persons from IOG IT\nteam, each one addressing different part of Hydra:"),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"One auditor is working on the project as a whole, targeting\npotential vulnerabilities with the off-chain code, the\ninfrastructure, dependencies, etc. This has not lead to any\nsignificant issue nor action plan so far."),(0,o.kt)("li",{parentName:"ul"},"Another auditor is specifically targeting the formal\nspecification and the on-chain code to identify vulnerabilities\nin the protocol itself."))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Discussions with researchers have led to the development of a joint Coordinated Hydra Head V1 specification defining formally the protocol as it is actually implemented."),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Work on this document has already allowed us to identify gaps"),(0,o.kt)("li",{parentName:"ul"},"It is the basis on which BCryptic's analyst is working to audit Hydra protocol"),(0,o.kt)("li",{parentName:"ul"},'It allows us to make explicit a lot of implicit assumptions that are in the code but not in the original paper and "Shape" the language used to describe the protocol'))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"We have drafted an RFP for submission to ",(0,o.kt)("strong",{parentName:"p"},"external")," auditors\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/blob/audit/rfp/security/RFP.md"},"https://github.com/input-output-hk/hydra/blob/audit/rfp/security/RFP.md")," defining the scope and targets of the audit")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"We are still unsure about whether our ",(0,o.kt)("strong",{parentName:"p"},"approach"),' is the right one as having a "formal specification" in a manually checked document spanning a dozen pages seems quite brittle.'),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},'Some progress has been made on "formalizing" properties to be automatically "QuickChecked" using an executable model of the expected behavior of the system but this approach seems more suited for "team-internal consumption", e.g. building confidence within the core committers and contributors')),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Ideally, we would want a proper formalization of the protocol, using an existing theorem proving/model checking framework, through which properties could be asserted. Quite a lot of work has already been done in blockchain space, including some work on Lightning and TLA+ or some other state-machine/temporal logic based language appear promising:"),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://www.youtube.com/watch?v=wecVT_4QDcU"},"https://www.youtube.com/watch?v=wecVT_4QDcU")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://github.com/rberenguel/tla_lightning"},"https://github.com/rberenguel/tla_lightning")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://www.amazon.com/Practical-TLA-Planning-Driven-Development/dp/1484238281"},"https://www.amazon.com/Practical-TLA-Planning-Driven-Development/dp/1484238281")))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"This effort should be done in accordance to the DApps certification process"))))),(0,o.kt)("h2",{id:"product"},"Product"),(0,o.kt)("p",null,"Most updates on the product side of things have been addressed in the\nstarmap update above."),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Hydra for Payments lighthouse project"),': Latest demonstration\nshown by Obsidian Systems on how to use payment channels in a "light\nway" from a web frontend. The API evolved and we were able to use\nthe ',(0,o.kt)("inlineCode",{parentName:"p"},"preview")," testnet now. The project is progressing very nicely\nand we are optimistic to close it successfully soon with improved\ndocumentation and usability.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"NFT auctions lighthouse project:")," After doing surveys and\ninterviews, MLabs is currently concluding the discovery phase with a\nlight / white paper on how NFT auctions could be implemented using\nHydra (today or in the future with more features).")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Drafting Voting on Hydra project"),": Work has started on building a\nProof-of-concept for voting on Hydra targeting Catalyst\u2019s use case but with an eye towards building a generic solution suitable for large scale voting systems based on Hydra Head"),(0,o.kt)("p",{parentName:"li"},"This development should be undertaken jointly with the Cardano Foundation and SundaeSwap who are also interested in building such a system and recently demonstrated their capacity of doing voting via the Cardano Summit voting system.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"TxPipe demonstrated integration with demeter.run:")," Without our\nhelp nor even knowing of this effort, TxPipe has recently shown an\nearly prototype of ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node"),"s instrumented via their\n",(0,o.kt)("a",{parentName:"p",href:"https://demeter.run"},"demeter.run")," platform. This has come a bit as a surprise and is exactly why we love open source \u2764\ufe0f \u2192 ",(0,o.kt)("a",{parentName:"p",href:"https://www.loom.com/share/c811360e60084f18ab9e9f16cc941432"},"Video")))),(0,o.kt)("h2",{id:"team--open-source"},"Team & open source"),(0,o.kt)("p",null,"Some notable developments this month have been:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Renamed the repository:")," from ",(0,o.kt)("strong",{parentName:"p"},"hydra-poc")," to\n",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra"},(0,o.kt)("strong",{parentName:"a"},"hydra")),"! This was revealed in the summit presentation (see below) and should indicate that the project is not only a Proof of Concept (POC) anymore, but has become more - as also demonstrated by the various early adopters and demos lately. \ud83d\udc32")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Inner source (IOG) contribution:")," The education team has been working on a Hydra Tutorial and we have been involved in reviewing and trying it out. This is a great effort and will help people get started with Hydra. Thanks folks \ud83d\udc9a"))),(0,o.kt)("h2",{id:"cardano-summit--workshop"},"Cardano Summit & workshop"),(0,o.kt)("p",null,"This month there was also the Cardano Summit, this time organized by the\nCardano Foundation and the content was even voted on by the community!\nWe have been both, invited and nominated as panelist and speakers and\nthis makes us extremely proud! In general, the reception of Hydra seems\nto be very positive in the community from what we could experience first\nhand on the summit."),(0,o.kt)("p",null,"Agenda items we participated in:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("a",{parentName:"p",href:"https://summit.cardano.org/agenda-day-1/best-of-blockchain-best-of-open-source/"},"Best of blockchain, best of open source - Open Source panel ")," with IOG, CF & TxPipe")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("a",{parentName:"p",href:"https://summit.cardano.org/agenda-day-2/cardano-ballot-speaker-winner-presentation-6/"},"Cardano Ballot Speaker Winner: Dev Team")," Introduction to Hydra and\nreveal of the repository rename \ud83c\udf89"))),(0,o.kt)("p",null,"After attending the summit, we also used the fact that the whole team is\nin one location for a team workshop. We booked a coworking space and\nspent 3 days together. Not much coding & hacking this time, but we are\nhappy to have produced these results:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Timeline / year in review:"),' As we had new team members with us,\nthe request was to give a recap of how the Hydra project evolved\nover the last two years. The whole timeline can bee seen below, and\nthis also sparked the idea of creating a "Year in review" blog post,\nwhich is currently in preparation.')),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Retrospective"),": In-person workshops are the perfect place for\ndoing retrospectives to reflect on what was good, bad and collecting\nideas & actions in how to improve our work environment and\nprocesses."),(0,o.kt)("p",{parentName:"li"},(0,o.kt)("img",{src:a(20148).Z,width:"4096",height:"2155"})),(0,o.kt)("small",null,(0,o.kt)("center",null,"Retrospective board from 2022-11-22"))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},(0,o.kt)("strong",{parentName:"p"},"Impact map:")," Conscious about the fact that the project got defined about one year ago in a first project plan, and inspired by timeline and (short-term) ideas, we also set off to reflect on the ",(0,o.kt)("strong",{parentName:"p"},"Why"),", ",(0,o.kt)("strong",{parentName:"p"},"How")," and ",(0,o.kt)("strong",{parentName:"p"},"What")," of this project."),(0,o.kt)("p",{parentName:"li"},"After reviewing the project vision, we reached for the tool of our choice to ideate on tangible objectives / deliverables - impact maps! Also, we felt the need to use a different goal this time. One that is reflecting more closely (or, in fact, broadly) what our current mission is. So instead of the ",(0,o.kt)("strong",{parentName:"p"},"% of Cardano transactions are done on L2"),", we set a new goal: being the ",(0,o.kt)("strong",{parentName:"p"},"number one DApp on Cardano")," (by all known metrics: TVL, traffic, volume etc..) Fundamentally, both goals illustrate the same idea, but the latter better supports the narrative that Hydra is also just a DApp (not a network upgrade) and needs usage & adoption to reach our vision."),(0,o.kt)("p",{parentName:"li"},"Below you see the result of our session. We checked back to the old impact map after creating this one, and many things are still relevant / similar on the new sample (it's never complete!) - some of the deliverables we even achieved."),(0,o.kt)("p",{parentName:"li"},(0,o.kt)("img",{src:a(41314).Z,width:"4096",height:"3449"})),(0,o.kt)("small",null,(0,o.kt)("center",null,"Impact map with new goal")))),(0,o.kt)("h1",{id:"conclusion"},"Conclusion"),(0,o.kt)("p",null,"Reflecting on our original plans (about a year ago) and our current roadmap we can say we are behind our anticipated schedule. However, recent developments on the lighthouse projects and community members contributing & using Hydra are convincing us that we are on the right track. The summit paints a similar picture and we feel reinvigorated by the great people we met, discussions we had and relationships we created & solidified on the summit and during our Hydra workshop."))}u.isMDXComponent=!0},41314:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/2022-11-impact-97ad611fd8b3395a70498f12d26703de.png"},20148:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/2022-11-retro-110daa66ba7449f00dc091e80b65df51.png"},19405:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/2022-11-roadmap-68fe47060bc543ac9013efeb0fd9335d.png"}}]);