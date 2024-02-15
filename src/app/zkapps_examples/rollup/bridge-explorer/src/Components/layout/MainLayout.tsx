import React from "react";
import { useLocation } from "react-router-dom";
import NavBar from "../UI/Molecules/NavBar/NavBar";

const noFooterOnSpecificRoutes = ["search"];

export default function MainLayout({ children }: { children: React.ReactNode }) {
  const location = useLocation();
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const [_, __, specificRoute] = location.pathname.split("/");

  return (
    <div className="flex flex-col">
      <NavBar />
      <main className="w-full flex-1">{children}</main>
      {!noFooterOnSpecificRoutes.includes(specificRoute) && (
        <footer className="flex justify-center pb-5"></footer>
      )}
    </div>
  );
}
