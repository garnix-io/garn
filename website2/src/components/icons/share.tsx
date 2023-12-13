import { SVGProps } from "react";

export const Share = (props: SVGProps<SVGSVGElement>) => (
  <svg viewBox="0 0 16 16" height="0.7em" width="0.7em" {...props}>
    <g strokeWidth="1.2" fill="none" stroke="currentColor">
      <path
        fill="none"
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeMiterlimit="10"
        d="M8.995,7.005 L8.995,7.005c1.374,1.374,1.374,3.601,0,4.975l-1.99,1.99c-1.374,1.374-3.601,1.374-4.975,0l0,0c-1.374-1.374-1.374-3.601,0-4.975 l1.748-1.698"
      ></path>
      <path
        fill="none"
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeMiterlimit="10"
        d="M7.005,8.995 L7.005,8.995c-1.374-1.374-1.374-3.601,0-4.975l1.99-1.99c1.374-1.374,3.601-1.374,4.975,0l0,0c1.374,1.374,1.374,3.601,0,4.975 l-1.748,1.698"
      ></path>
    </g>
  </svg>
);
