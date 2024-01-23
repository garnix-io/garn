(self.webpackChunk_N_E=self.webpackChunk_N_E||[]).push([[931],{33018:function(e,n,t){Promise.resolve().then(t.t.bind(t,68326,23)),Promise.resolve().then(t.t.bind(t,72512,23)),Promise.resolve().then(t.t.bind(t,62384,23)),Promise.resolve().then(t.t.bind(t,22323,23)),Promise.resolve().then(t.t.bind(t,10721,23)),Promise.resolve().then(t.t.bind(t,41736,23)),Promise.resolve().then(t.bind(t,24582)),Promise.resolve().then(t.bind(t,19815)),Promise.resolve().then(t.t.bind(t,87020,23)),Promise.resolve().then(t.t.bind(t,36417,23)),Promise.resolve().then(t.t.bind(t,28780,23)),Promise.resolve().then(t.t.bind(t,6021,23)),Promise.resolve().then(t.bind(t,37443))},24582:function(e,n,t){"use strict";t.r(n),t.d(n,{FAQ:function(){return g}});var r=t(57437),s=t(64472),a=t(34346),i=t.n(a),o=t(2265),c=t(61396),l=t.n(c),d=t(83754),h=t(25090),p=t(76499),_=t(36417),u=t.n(_);let x=[{question:"How does garn differ from docker, vagrant, etc.?",answer:(0,r.jsxs)(r.Fragment,{children:[(0,r.jsxs)("div",{className:i().paragraph,children:[(0,r.jsx)(s.x,{type:"proper",children:"garn"})," uses nix instead of containers or VMs to provide dependencies. These dependencies will all be installed into the nix store (in ",(0,r.jsx)(s.x,{type:"code",children:"/nix/store"}),") and will be provided from there in the environments that you create with"," ",(0,r.jsx)(s.x,{type:"proper",children:"garn"}),"."]}),(0,r.jsxs)("div",{className:i().paragraph,children:["That means that ",(0,r.jsx)(s.x,{type:"proper",children:"garn"})," scripts and checks and everything else you do when developing with"," ",(0,r.jsx)(s.x,{type:"proper",children:"garn"})," runs natively on your machine. So there is no overhead that you would get when using VMs or containers on non-linux hosts."]}),(0,r.jsxs)("div",{className:i().paragraph,children:["That also means that -- when entering an environment with"," ",(0,r.jsx)(s.x,{type:"code",children:"garn enter"})," -- you can use your personal editor or other globally installed tools with no additional work."]})]})},{question:"How does garn differ from 'npm', 'cabal' and other language-specific tools?",answer:(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(s.x,{type:"proper",children:"garn"})," is language agnostic. That means you can use it in projects that mix multiple languages and have the same experience and workflows for all sub-projects. It also means that programmers that are unfamiliar with the toolchain of a project can use"," ",(0,r.jsx)(s.x,{type:"proper",children:"garn"})," to get started quickly."]})},{question:"What languages does garn support?",answer:(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(s.x,{type:"proper",children:"garn"})," currently supports ",(0,r.jsx)("b",{children:"Go"}),", ",(0,r.jsx)("b",{children:"Npm"})," ","and ",(0,r.jsx)("b",{children:"Haskell"}),". If you'd like to see support for other languages or toolchains please"," ",(0,r.jsx)(l(),{className:i().link,href:"https://github.com/garnix-io/garn/issues",target:"_blank",children:"let us know"}),"."]})},{question:"How can I configure LSP to get error messages and auto-completion for garn.ts in my editor?",answer:(0,r.jsxs)(r.Fragment,{children:[(0,r.jsxs)("div",{className:i().paragraph,children:[(0,r.jsx)(s.x,{type:"code",children:"garn.ts"})," files are powered by"," ",(0,r.jsx)(l(),{className:i().link,href:"https://deno.com/",target:"_blank",children:"Deno"}),". A lot of the convenience and power for editing your"," ",(0,r.jsx)(s.x,{type:"code",children:"garn.ts"})," files comes from having a working Deno LSP. There are two ways of setting up LSP for editing"," ",(0,r.jsx)(s.x,{type:"code",children:"garn.ts"})," files: using"," ",(0,r.jsx)(s.x,{type:"code",children:"garn edit"}),", which provides a properly-configured editor, or using ",(0,r.jsx)(s.x,{type:"proper",children:"garn"})," ","to provide ",(0,r.jsx)(s.x,{type:"proper",children:"deno"})," and setting up your editor configuration yourself."]}),(0,r.jsx)("div",{className:i().paragraph,children:(0,r.jsx)(s.x,{type:"code",children:"garn edit"})}),(0,r.jsxs)("div",{className:i().paragraph,children:[(0,r.jsx)(s.x,{type:"proper",children:"garn"})," can download and configure"," ",(0,r.jsx)(l(),{className:i().link,href:"https://vscodium.com/",children:"vscodium"})," ","for you — just type ",(0,r.jsx)(s.x,{type:"code",children:"garn edit"}),". It will spin up a ",(0,r.jsx)(s.x,{type:"proper",children:"vscodium"})," editor that is pre-configured for editing ",(0,r.jsx)(s.x,{type:"code",children:"garn.ts"})," files. It won't use or modify your local vscodium settings, if you have any. You can can add it to your ",(0,r.jsx)(s.x,{type:"code",children:"garn.ts"})," file like this:"]}),(0,r.jsx)("div",{className:i().paragraph,children:(0,r.jsx)(s.x,{type:"h3",children:"Installing the Deno LSP and configuring your editor"})}),(0,r.jsxs)("div",{className:i().paragraph,children:["Alternatively you can install deno (including the Deno LSP) with"," ",(0,r.jsx)(s.x,{type:"proper",children:"garn"})," itself:"]}),(0,r.jsx)("div",{className:i().paragraph,children:(0,r.jsx)(e=>{let{title:n,text:t,expectedLineCount:s,inverse:a,className:i}=e;return(0,r.jsxs)("div",{className:"".concat(u().container," ").concat(a?u().inverse:""," ").concat(i),children:[n&&(0,r.jsx)("div",{className:u().header,children:n}),(0,r.jsx)("pre",{className:u().content,style:s?{minHeight:28*s}:{},children:t})]})},{inverse:!0,title:"garn.ts",text:(0,r.jsx)(e=>{let{code:n,inverse:t,language:s}=e;return(0,r.jsx)(d.Z,{language:s||"javascript",style:t?h.Z:p.Z,customStyle:{background:"transparent",padding:0,margin:0,textShadow:"none"},codeTagProps:{style:{background:"transparent"}},children:n})},{code:'import * as garn from "https://garn.io/ts/v0.0.20/mod.ts";\n\nexport const deno = garn.mkProject({\n  description: "garn configuration environment",\n  defaultEnvironment: garn.emptyEnvironment.withDevTools([pkgs.deno]),\n}, {});',inverse:!0})})}),(0,r.jsxs)("div",{className:i().paragraph,children:[(0,r.jsx)(s.x,{type:"proper",children:"garn"})," enter deno will then drop you in a shell where ",(0,r.jsx)(s.x,{type:"proper",children:"deno"})," is available."]}),(0,r.jsxs)("div",{className:i().paragraph,children:["For configuring your editor to use Deno's LSP refer to"," ",(0,r.jsx)(l(),{className:i().link,href:"https://docs.deno.com/runtime/manual/getting_started/setup_your_environment",children:"Deno's environment setup documentation"}),"."]})]})}],g=()=>{let[e,n]=(0,o.useState)();return(0,r.jsx)("section",{className:i().container,children:(0,r.jsxs)("div",{className:i().content,children:[(0,r.jsx)(s.x,{className:i().title,type:"h2",children:"Faq"}),x.map((t,a)=>(0,r.jsxs)("div",{className:i().faq,onClick:()=>n(e===a?void 0:a),children:[(0,r.jsxs)("div",{className:i().faqHeader,children:[(0,r.jsx)(s.x,{className:i().faqTitle,type:"h3",children:t.question}),(0,r.jsx)("div",{className:"".concat(i().toggle," ").concat(e===a?i().open:""),children:"+"})]}),(0,r.jsx)(s.x,{type:"span",className:"".concat(i().faqAnswer," ").concat(e===a?i().open:""),children:t.answer})]},a))]})})}},19815:function(e,n,t){"use strict";t.r(n),t.d(n,{Social:function(){return g}});var r=t(57437),s=t(16691),a=t.n(s),i=t(63739),o=t(64472),c=t(41736),l=t.n(c),d=t(61396),h=t.n(d);let p=e=>{let{title:n,links:t,className:s=""}=e;return(0,r.jsxs)("div",{className:"".concat(l().section," ").concat(s),children:[(0,r.jsx)(o.x,{type:"h1",className:l().header,children:n}),(0,r.jsx)("div",{className:l().actions,children:t.map((e,n)=>(0,r.jsxs)(h(),{className:l().action,href:e.href,target:e.href.includes("http")?"_blank":"",children:[(0,r.jsx)("div",{className:l().text,children:e.text}),(0,r.jsx)("div",{className:l().arrow,children:"→"})]},n))})]})};var _={src:"/_next/static/media/shape.96d15fb1.svg",height:837,width:809,blurWidth:0,blurHeight:0},u=t(4715),x=t.n(u);let g=()=>{let{width:e}=(0,i.iP)();return(0,r.jsxs)("section",{className:x().container,children:[(0,r.jsx)(p,{className:x().header,title:"Experience real connections and support in our virtual talk circles",links:[{text:"Discord",href:"https://discord.gg/XtDrPsqpVx"},{text:"Github",href:"https://github.com/garnix-io/garn"}]}),(0,r.jsx)(a(),{src:_,alt:"social background d20",className:x().backgroundShape,fill:e<=850})]})}},64472:function(e,n,t){"use strict";t.d(n,{x:function(){return Text}});var r=t(57437),s=t(74578),a=t(55408),i=t.n(a);let o={className:i().MatterSQMono},c={className:i().Berlin};var l=t(87020),d=t.n(l),h=t(39054);let p=s.ZP.object({id:s.ZP.string().optional(),type:s.ZP.enum(["p","h1","h2","h3","proper","code","span"]).optional(),className:s.ZP.string().optional(),children:s.ZP.custom()}),Text=(0,h.W)(p,e=>{let{type:n="p",children:t,className:s,...a}=e;if("p"===n)return(0,r.jsx)("p",{...a,className:"".concat(c.className," ").concat(d().paragraph," ").concat(s),children:t});if("h1"===n)return(0,r.jsx)("h1",{...a,className:"".concat(d().header," ").concat(s),children:t});if("h2"===n)return(0,r.jsx)("h2",{...a,className:"".concat(d().header," ").concat(s),children:t});if("h3"===n)return(0,r.jsx)("h3",{...a,className:"".concat(d().header," ").concat(d().header3," ").concat(s),children:t});if("proper"===n)return(0,r.jsx)("span",{...a,className:"".concat(d().proper," ").concat(s," ").concat(o.className),children:t});if("code"===n)return(0,r.jsx)("span",{...a,className:"".concat(d().code," ").concat(s," ").concat(o.className),children:t});if("span"===n)return(0,r.jsx)("span",{...a,className:"".concat(c.className," ").concat(d().paragraph," ").concat(s),children:t});else return(0,r.jsxs)("div",{children:["Text type not found: ",n]})})},37443:function(e,n,t){"use strict";t.r(n),t.d(n,{TypingText:function(){return o}});var r=t(57437),s=t(2265),a=t(36387),i=t.n(a);let o=e=>{let{prependedText:n,commands:t}=e,[a,o]=(0,s.useState)({idx:0,lines:[],currentLine:""}),c=t[a.idx];(0,s.useEffect)(()=>{let e;let t=(n,t)=>{e=setTimeout(()=>{o({...a,...t})},n)};if("delay"===c.action)t(c.delay||1e3,{idx:a.idx+=1});else if("type"===c.action){if(0===a.currentLine.length)t(c.delay||50,{currentLine:"".concat(n," ")});else if(a.currentLine.length<c.text.length+n.length+1){let e=c.text.slice(a.currentLine.length-n.length-1,a.currentLine.length-n.length);t(20+45*Math.random(),{currentLine:"".concat(a.currentLine).concat(e)})}else t(100,{idx:a.idx+=1,currentLine:"",lines:[...a.lines,a.currentLine]})}else"delete"===c.action?0===a.currentLine.length?t(c.delay||50,{lines:a.lines.slice(0,a.lines.length-1),currentLine:a.lines[a.lines.length-1]}):a.currentLine.length<=n.length+1?t(100,{idx:a.idx+=1}):t(5+15*Math.random(),{currentLine:a.currentLine.slice(0,a.currentLine.length-1)}):"response"===c.action?t(c.delay||100,{lines:[...a.lines,c.text],idx:a.idx+=1}):"clear"===c.action?t(c.delay||100,{lines:[],idx:a.idx+=1}):"restart"===c.action&&t(c.delay||100,{idx:0});return()=>clearTimeout(e)});let l=a.currentLine.length>0?[...a.lines,a.currentLine]:a.lines;return(0,r.jsxs)("pre",{className:i().container,children:[l.join("\n"),(["type","delete"].includes(c.action)&&a.currentLine.length>0||c.showCursor)&&(0,r.jsx)("div",{className:i().cursor})]})}},39054:function(e,n,t){"use strict";t.d(n,{W:function(){return r}});let r=(e,n)=>t=>n(e.strict().parse(t))},72512:function(e){e.exports={container:"styles_container__Kpx_8",garnix:"styles_garnix__ivFRw"}},62384:function(e){e.exports={container:"styles_container__07AW5",section:"styles_section__vAok0",sectionMobile:"styles_sectionMobile__0tM7O",link:"styles_link___WqYd",sectionDesktop:"styles_sectionDesktop__SuhIB"}},41736:function(e){e.exports={section:"styles_section__3Tggi",header:"styles_header__iMWaP",actions:"styles_actions__nPIwF",action:"styles_action__H2T72",text:"styles_text__rot9s",arrow:"styles_arrow__Qkefv",arrowBounce:"styles_arrowBounce__lIDrX"}},22323:function(e){e.exports={container:"styles_container__Ri1AJ",textContainer:"styles_textContainer__5qNc2",terminalContainer:"styles_terminalContainer__AeHH8",header:"styles_header__3YqTg"}},10721:function(e){e.exports={container:"styles_container__cf0S6",header:"styles_header__FHByM",headerTitle:"styles_headerTitle__KRFEv",headerText:"styles_headerText__jeaoU",terminalContainer:"styles_terminalContainer__WuNAu"}},34346:function(e){e.exports={container:"styles_container__1g4o6",content:"styles_content__S5Ul7",title:"styles_title__249ZT",faq:"styles_faq__Np3_V",paragraph:"styles_paragraph__JpuyA",link:"styles_link__a7v_F",faqTitle:"styles_faqTitle__jzZ52",toggle:"styles_toggle__TgFv7",faqHeader:"styles_faqHeader__0dx4Y",open:"styles_open__GDEJd",faqAnswer:"styles_faqAnswer__IHpEL"}},6021:function(e){e.exports={container:"styles_container__eEfj8",content:"styles_content__FnKIs",mobileDivider:"styles_mobileDivider___AuEw"}},4715:function(e){e.exports={container:"styles_container__H_wjB",header:"styles_header__mc505",backgroundShape:"styles_backgroundShape___gZR6"}},28780:function(e){e.exports={container:"styles_container__yQpgo",header:"styles_header__dREo7",headerTitle:"styles_headerTitle__ErOQc",terminalContainer:"styles_terminalContainer__hZTUF"}},36417:function(e){e.exports={container:"styles_container__ySt5O",inverse:"styles_inverse___AS0P",header:"styles_header___Kqbc",content:"styles_content__FnlSt"}},87020:function(e){e.exports={paragraph:"styles_paragraph__osJR5",header:"styles_header__uOOWl",header3:"styles_header3__fw_Of",proper:"styles_proper__VIgAS",code:"styles_code__lZCKD"}},36387:function(e){e.exports={container:"styles_container__cB4Tb",cursor:"styles_cursor__ufHvv"}},55408:function(e){e.exports={MatterSQMono:"fonts_MatterSQMono__sNqUK",Berlin:"fonts_Berlin__70d7E"}}},function(e){e.O(0,[82,97,971,472,744],function(){return e(e.s=33018)}),_N_E=e.O()}]);