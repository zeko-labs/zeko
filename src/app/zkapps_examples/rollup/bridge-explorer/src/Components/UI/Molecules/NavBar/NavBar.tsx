import { Fragment } from "react";
import { Link, useMatch } from "react-router-dom";
import { ReactComponent as CloseIcon } from "../../../../Assets/close-icon.svg";
import { ReactComponent as ZekoSmallIcon } from "../../../../Assets/zeko-small-icon.svg";
import ChangeNetwork from "../../Atoms/ChangeNetwork/ChangeNetwork";
import { ReactComponent as MenuIcon } from "../../../../Assets/icon_hamburger.svg";
import { Popover, Transition } from "@headlessui/react";
import { useAppProvider } from "../../../../context/AppContext";
import clsx from "clsx";
import NavbarLogo from "../../../../Assets/logos/navbarLogo.svg";

function NavLink({ to, children }: { to: string; children: string }) {
  const match = useMatch(to);

  return (
    <Link
      key={to}
      to={to}
      className={clsx(
        "flex h-[40px] items-center rounded-md px-5 text-sm font-semibold text-white transition duration-300 ease-out hover:bg-gray-800 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-orange-600",
        match ? "underline" : "no-underline"
      )}
    >
      {children}
    </Link>
  );
}
function NavLinkMobile({
  to,
  children,
  onClick,
}: {
  to: string;
  children: string;
  onClick: () => void;
}) {
  const match = useMatch(to);

  return (
    <Link
      key={to}
      to={to}
      onClick={onClick}
      className={clsx(
        "hover:bg-gradient-secondary block rounded-md px-5 py-3 text-sm font-semibold text-white focus:outline-none focus:ring-2 focus:ring-inset focus:ring-orange-600",
        match ? "underline" : "no-underline"
      )}
    >
      {children}
    </Link>
  );
}

function NavBar() {
  const { selectedNetwork } = useAppProvider();

  // define navigation here
  const navigation = [
    { name: "Home", route: `/${selectedNetwork.id}` },
    { name: "Search", route: `/${selectedNetwork.id}/search` },
  ];

  return (
    <div className="bg-gradient-secondary fixed z-50 w-full shadow-default">
      <div className="mx-auto max-w-7xl px-5">
        <Popover>
          <div className="flex h-[75px] items-center justify-between">
            <nav className="flex w-full" aria-label="Global">
              <div className="flex flex-grow items-center">
                <div className="flex w-full items-center justify-between md:w-auto">
                  <Link className="flex items-center" to={`/${selectedNetwork.id}`}>
                    <img
                      className="mr-3 hidden h-5 w-auto md:block md:h-5"
                      src={NavbarLogo}
                      alt="logo"
                    />
                    <ZekoSmallIcon className="block md:hidden" />
                  </Link>
                  <div className="flex items-center md:hidden">
                    <div className="pr-2">
                      <ChangeNetwork />
                    </div>
                    <Popover.Button className="inline-flex items-center justify-center rounded-md focus:outline-none focus:ring-2 focus:ring-inset focus:ring-orange-600">
                      <span className="sr-only">Open main menu</span>
                      <MenuIcon className="h-10 w-10" aria-hidden="true" />
                    </Popover.Button>
                  </div>
                </div>
              </div>
              <div className="hidden items-center space-x-1 md:ml-10 md:flex md:pr-4">
                {navigation.map((item) => (
                  <NavLink key={item.name} to={item.route}>
                    {item.name}
                  </NavLink>
                ))}
              </div>
              <ChangeNetwork className="hidden md:flex" />
            </nav>
          </div>

          <Transition
            as={Fragment}
            enter="duration-150 ease-out"
            enterFrom="opacity-0 scale-95"
            enterTo="opacity-100 scale-100"
            leave="duration-100 ease-in"
            leaveFrom="opacity-100 scale-100"
            leaveTo="opacity-0 scale-95"
          >
            <Popover.Panel
              focus
              className="absolute inset-x-0 top-0 z-20 origin-top-right transform p-2 transition md:hidden"
            >
              {({ close }) => (
                <div className="h-full rounded-lg bg-gray-800 shadow-lg ring-1 ring-black ring-opacity-5">
                  <div className="flex items-center justify-between px-5 pt-4">
                    <div>
                      <Link className="flex items-center" to={selectedNetwork.id}>
                        <img className="mr-3 h-5 w-auto" src={NavbarLogo} alt="logo" />
                      </Link>
                    </div>

                    <div className="-mr-2">
                      <Popover.Button className="inline-flex items-center justify-center rounded-md p-2 text-gray-400 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-orange-600">
                        <span className="sr-only">Close main menu</span>
                        <CloseIcon className="h-6 w-6" aria-hidden="true" />
                      </Popover.Button>
                    </div>
                  </div>
                  <div className="space-y-0.5 px-2 pt-2 pb-3">
                    {navigation.map((item) => (
                      <NavLinkMobile key={item.name} to={item.route} onClick={() => close()}>
                        {item.name}
                      </NavLinkMobile>
                    ))}
                  </div>
                </div>
              )}
            </Popover.Panel>
          </Transition>
        </Popover>
      </div>
    </div>
  );
}

export default NavBar;
