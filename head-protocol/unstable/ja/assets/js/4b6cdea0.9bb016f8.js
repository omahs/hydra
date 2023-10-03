"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[3498],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>m});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=a.createContext({}),p=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},d=function(e){var t=p(e.components);return a.createElement(l.Provider,{value:t},e.children)},h="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},c=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,l=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),h=p(n),c=r,m=h["".concat(l,".").concat(c)]||h[c]||u[c]||o;return n?a.createElement(m,i(i({ref:t},d),{},{components:n})):a.createElement(m,i({ref:t},d))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=c;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[h]="string"==typeof e?e:r,i[1]=s;for(var p=2;p<o;p++)i[p]=n[p];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}c.displayName="MDXCreateElement"},10788:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>s,toc:()=>p});var a=n(87462),r=(n(67294),n(3905));const o={title:"August 2023",slug:"2023-08",authors:["pgrange","v0d1ch","ffakenz","ch1bo"],tags:["monthly"]},i=void 0,s={permalink:"/head-protocol/unstable/ja/monthly/2023-08",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/monthly/2023-08-monthly.md",source:"@site/monthly/2023-08-monthly.md",title:"August 2023",description:"This report summarizes the work on Hydra since July 2023. It serves as",date:"2023-08-30T13:35:03.000Z",formattedDate:"2023\u5e748\u670830\u65e5",tags:[{label:"monthly",permalink:"/head-protocol/unstable/ja/monthly/tags/monthly"}],readingTime:5.92,hasTruncateMarker:!1,authors:[{name:"Pascal Grange",title:"Senior Software Engineer",url:"https://github.com/pgrange",imageURL:"https://github.com/pgrange.png",key:"pgrange"},{name:"Sasha Bogicevic",title:"Senior Software Engineer",url:"https://github.com/v0d1ch",imageURL:"https://github.com/v0d1ch.png",key:"v0d1ch"},{name:"Franco Testagrossa",title:"Senior Software Engineer",url:"https://github.com/ffakenz",imageURL:"https://github.com/ffakenz.png",key:"ffakenz"},{name:"Sebastian Nagel",title:"Software Engineering Lead",url:"https://github.com/ch1bo",imageURL:"https://github.com/ch1bo.png",key:"ch1bo"}],frontMatter:{title:"August 2023",slug:"2023-08",authors:["pgrange","v0d1ch","ffakenz","ch1bo"],tags:["monthly"]},prevItem:{title:"September 2023",permalink:"/head-protocol/unstable/ja/monthly/2023-09"},nextItem:{title:"July 2023",permalink:"/head-protocol/unstable/ja/monthly/2023-07"}},l={authorsImageUrls:[void 0,void 0,void 0,void 0]},p=[{value:"Roadmap",id:"roadmap",level:2},{value:"Release 0.12.0",id:"release-0120",level:4},{value:"Development",id:"development",level:2},{value:"Update the tutorial and include Mithril #997",id:"update-the-tutorial-and-include-mithril-997",level:4},{value:"Support cardano-node 8.1.2 #1007",id:"support-cardano-node-812-1007",level:4},{value:"Event-sourced persistence #913",id:"event-sourced-persistence-913",level:4},{value:"New API endpoints",id:"new-api-endpoints",level:4},{value:"Removal of &#39;internal commit&#39; endpoint #1018",id:"removal-of-internal-commit-endpoint-1018",level:4},{value:"Community",id:"community",level:2},{value:"Hydra master class at RareEvo",id:"hydra-master-class-at-rareevo",level:4},{value:"Catalyst Fund10",id:"catalyst-fund10",level:4},{value:"Conclusion",id:"conclusion",level:2}],d={toc:p},h="wrapper";function u(e){let{components:t,...o}=e;return(0,r.kt)(h,(0,a.Z)({},d,o,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"This report summarizes the work on Hydra since July 2023. It serves as\npreparation for the monthly review meeting (see ",(0,r.kt)("a",{parentName:"p",href:"https://docs.google.com/presentation/d/1MrCeUsYb3FQk7aCwMZdQs8mc5BfLOIjkK9gcWzgDdDc/edit#slide=id.g1f87a7454a5_0_1392"},"slides")," and\n",(0,r.kt)("a",{parentName:"p",href:"https://drive.google.com/file/d/14pDsf0hDyh9HK8sCSMmkmT8gY8YxgOQ8/view"},"recording"),"), where the team updates project stakeholders on recent\ndevelopments to gather their feedback on proposed plans."),(0,r.kt)("h2",{id:"roadmap"},"Roadmap"),(0,r.kt)("p",null,"This month, the team released version 0.12.0, and the project\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/orgs/input-output-hk/projects/21/views/7"},"roadmap")," has been\nslightly updated to focus 0.13.0 on network resiliency and bump incremental\ncommit and decommit in priority:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"The roadmap with features and ideas",src:n(70519).Z,width:"2048",height:"682"})," ",(0,r.kt)("small",null,(0,r.kt)("center",null,"The latest roadmap with features and ideas"))),(0,r.kt)("h4",{id:"release-0120"},"Release 0.12.0"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Support cardano-node 8.1.2"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Updated client and Plutus versions"))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Layer 2 protocol changes"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Authenticated messages"),(0,r.kt)("li",{parentName:"ul"},"Removed redundancy"))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Event-sourced persistence")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"New API endpoints")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Removal of ",(0,r.kt)("em",{parentName:"p"},"internal commit")," endpoint")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Improved off-chain transaction processing performance")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Security fixes")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"See ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/releases/tag/0.12.0"},"full release notes")," and a list of ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/milestone/12?closed=1"},"delivered features")))),(0,r.kt)("h2",{id:"development"},"Development"),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-07-28..2023-08-29"},"Issues and pull requests closed since the last\nreport")),(0,r.kt)("p",null,"This month, the team worked on the following:"),(0,r.kt)("h4",{id:"update-the-tutorial-and-include-mithril-997"},"Update the tutorial and include Mithril ",(0,r.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/issues/997"},"#997")),(0,r.kt)("p",null,"To prepare for the Hydra master class at RareEvo, the team has taken the\noriginally written tutorial for Hydra version 0.8.0 and updated it to work with the latest\nversions."),(0,r.kt)("p",null,"The challenge was to write content that is both easily comprehensible and functional across a wide\nrange of user platforms, including operating system and processor architectures."),(0,r.kt)("p",null,"Besides writing the tutorial, it is essential to ensure that it is kept\nup-to-date (eg, using continuous integration)."),(0,r.kt)("h4",{id:"support-cardano-node-812-1007"},"Support cardano-node 8.1.2 ",(0,r.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/issues/1007"},"#1007")),(0,r.kt)("p",null,"To be able to use the latest Mithril snapshots to bootstrap a\n",(0,r.kt)("inlineCode",{parentName:"p"},"cardano-node")," for the ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node"),", we needed to make some updates."),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," uses the ",(0,r.kt)("inlineCode",{parentName:"p"},"cardano-api")," to establish a connection with the node using the node-to-client protocols. The format there has slightly changed (although versioned), necessitating an update to the version of ",(0,r.kt)("inlineCode",{parentName:"p"},"cardano-api")," used within the ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node"),"."),(0,r.kt)("p",null,"The way Haskell dependencies are managed required an adjustment of the versions for both ",(0,r.kt)("inlineCode",{parentName:"p"},"cardano-ledger")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"plutus"),". These versions are used to construct our off-chain ledger and on-chain Hydra Head protocol scripts, respectively."),(0,r.kt)("p",null,"As a result, this process has proven to be more intricate than it might initially sound. However, it has ultimately resulted in enhancements to the efficiency of our on-chain scripts."),(0,r.kt)("p",null,"TODO: how much?"),(0,r.kt)("h4",{id:"event-sourced-persistence-913"},"Event-sourced persistence ",(0,r.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/issues/913"},"#913")),(0,r.kt)("p",null,"We want the hydra-node to be efficient in processing events to yield high\nthroughput on processing transactions off-chain."),(0,r.kt)("p",null,"Work done as part of ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/186"},"#186"),"\nhas demonstrated that the primary bottleneck to faster transaction processing\ninside the node was the state persistence logic, which simply overwrites the\nfull state with whatever new state has been produced."),(0,r.kt)("p",null,"For that reason, we changed the persistent state to a sequence of events\naccording to ",(0,r.kt)("a",{parentName:"p",href:"/adr/24"},"ADR24"),". Persistence is now done incrementally by saving\nonly the ",(0,r.kt)("inlineCode",{parentName:"p"},"StateChanged")," deltas."),(0,r.kt)("p",null,"As a consequence, the first spike confirmed the following performance\nimprovements: master ~300ms \u2192 spike ~6ms."),(0,r.kt)("p",null,"TODO: what are the numbers on master before/after?"),(0,r.kt)("p",null,"Finally, this also opens up interesting possibilities for state observation in\nclients."),(0,r.kt)("h4",{id:"new-api-endpoints"},"New API endpoints"),(0,r.kt)("p",null,"This release also includes several additions to the Hydra API. We added the\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/1001"},"/cardano-transaction\nendpoint")," to submit a\ntransaction to the layer 1 network. This feature improves developer experience as\nHydra clients do not need direct chain access (eg, connect to ",(0,r.kt)("inlineCode",{parentName:"p"},"cardano-node"),")\nto be able to submit transactions."),(0,r.kt)("p",null,"The other new ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/989"},"/protocol-parameters\nendpoint")," serves the\ncurrently configured protocol parameters used in ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node"),". This provides\nmore flexibility when creating transactions for the head on the client side and\navoids configuration or hard-coded values."),(0,r.kt)("p",null,"On top of this, we also included the hydra-node\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/985"},"version")," inside of the\n",(0,r.kt)("inlineCode",{parentName:"p"},"Greetings")," message. This is very useful for debugging purposes and detecting\npossible version mismatches."),(0,r.kt)("h4",{id:"removal-of-internal-commit-endpoint-1018"},"Removal of 'internal commit' endpoint ",(0,r.kt)("a",{parentName:"h4",href:"https://github.com/input-output-hk/hydra/pull/1018"},"#1018")),(0,r.kt)("p",null,"In the previous release, we made an announcement regarding the deprecation of committing to the head through the websocket command. Subsequently, we have taken steps to eliminate this client command, which in turn resulted in the removal of the ",(0,r.kt)("em",{parentName:"p"},"fuel")," markers that were previously used to mark the UTXO of the internal Hydra wallet meant for use in the Head."),(0,r.kt)("p",null,"This simplifies the setup needed to run the Head protocol and improves security\nsince users can directly commit funds from their wallets without sending them to\nthe Head operator beforehand."),(0,r.kt)("h2",{id:"community"},"Community"),(0,r.kt)("h4",{id:"hydra-master-class-at-rareevo"},"Hydra master class at RareEvo"),(0,r.kt)("p",null,"We were happy to run a Hydra master class session at RareEvo 2023. The session\nattracted 30+ attendees for the introductory parts including a presentation on\nHydra and Mithril. About 10-15 participants remained for the practical part and\nfollowing discussion."),(0,r.kt)("p",null,"We also streamed the event live on Discord, but were not able to interact much\nthere. The responses were positive though and we should be doing more things in\nthe public on this #hydra-live channel."),(0,r.kt)("p",null,"Several participants managed to use Mithril and synchronize a pre-production cardano-node; two teams effectively initiated Hydra heads, conducted fund transactions within them, and subsequently closed them. The major challenges, as expected, concerned installation and networking. In the future, we intend to ensure the availability of prebuilt binaries catering to a wider range of platforms."),(0,r.kt)("h4",{id:"catalyst-fund10"},"Catalyst Fund10"),(0,r.kt)("p",null,"The team screened all the proposals mentioning Hydra and\n",(0,r.kt)("a",{parentName:"p",href:"https://mithril.network/doc/"},"Mithril"),". We submitted 11 community reviews and\nnoticed, in particular, the following proposals:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://cardano.ideascale.com/c/idea/102138"},"Sundae Labs Hydra Ledger-only Mode")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://cardano.ideascale.com/c/idea/102200"},"Sundae Labs Hydra Transaction Stream Plugin")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://cardano.ideascale.com/c/idea/101626"},"Hydra as a B2B layer for DeFi - a white paper and an MVP")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://cardano.ideascale.com/c/idea/104411"},"Decentralized Demeter.run - Federated Frontend Hosting - New revenue stream for SPOs")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("a",{parentName:"li",href:"https://cardano.ideascale.com/c/idea/105113"},"Mithril - Open-source contributor"))),(0,r.kt)("h2",{id:"conclusion"},"Conclusion"),(0,r.kt)("p",null,"The monthly review meeting for August 2023 was held on 2023-08-23 via Google\nMeet with these ",(0,r.kt)("a",{parentName:"p",href:"https://docs.google.com/presentation/d/1MrCeUsYb3FQk7aCwMZdQs8mc5BfLOIjkK9gcWzgDdDc/edit#slide=id.g1f87a7454a5_0_1392"},"slides")," and the ",(0,r.kt)("a",{parentName:"p",href:"https://drive.google.com/file/d/14pDsf0hDyh9HK8sCSMmkmT8gY8YxgOQ8/view"},"recording"),", 'broadcasting live from warm and sunny Colorado'!"),(0,r.kt)("p",null,"It has been an interesting and unusual month. Some of the team had been in\nLongmont, CO to prepare for the RareEvo event and we used the chance to have the\nmeeting in a hybrid setting with some IO stakeholders attending live on-site and\nabout 20 community members online."),(0,r.kt)("p",null,"This time, the demo was about the updated tutorial and demonstrating the full\nsetup of the cardano-node, opening a Hydra head on the pre-production network, and submitting\ntransactions off-chain in 15 minutes!"),(0,r.kt)("p",null,"The feedback we received included inquiries about the timing, method, and extent of the audit for the Head protocol. While we will have an internal audit, which is already\nhelping us improve the protocol, there are no plans for a significant external audit with funding. We also had the chance to look into and learn about some Catalyst Fund10\nproposals involving Hydra. Hopefully, some or all of them get funded and we are\nlooking forward to testing the Hydrazoa concept, implementing the ledger-mode operation, enabling federated Heads, and achieving other objectives."),(0,r.kt)("p",null,"At the RareEvo event, we had the chance to meet and communicate with various people\nfrom the community. This ranges from known Hydra collaborators to tech-savvy\nstake pool operators, to representatives of successful applications running on\nCardano for scaling purposes like ",(0,r.kt)("a",{parentName:"p",href:"https://book.io/"},"book.io"),"."),(0,r.kt)("p",null,"Also with a new full-time contributor on board, we are keen to add more\nfunctionality while the first applications prepare to utilize Hydra in production\non mainnet."))}u.isMDXComponent=!0},70519:(e,t,n)=>{n.d(t,{Z:()=>a});const a=n.p+"assets/images/2023-08-roadmap-8a47097a86ba8dc4a2a48767414fa548.jpg"}}]);