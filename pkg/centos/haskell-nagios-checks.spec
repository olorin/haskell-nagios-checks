Name:		haskell-nagios-checks
Version:	1.0
Release:	0.0anchor%{?build_number}%{!?build_number:1}%{?dist}
Summary:	Various nagios checks written in Haskell.

Group:		Development/Libraries
License:	BSD
URL:		https://github.com/anchor/haskell-nagios-checks
Source0:	haskell-nagios-checks-%{version}.tar.gz
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:	ghc >= 7.8.3
BuildRequires:	cabal-install
Requires:	gmp

%description
haskell-nagios-checks is a collection of Nagios checks written in the Haskell
programming language.

* check-page-fragmentation reports memory fragmentation reported by the kernel.
* check-rabbitmq-exchange reports throughput of a specified RabbitMQ exchange.

%global ghc_without_dynamic 1

%prep
%setup

%build

%install
mkdir -p %{buildroot}%{_bindir}
install -m 0755 check-page-fragmentation %{buildroot}%{_bindir}
install -m 0755 check-rabbitmq-exchange %{buildroot}%{_bindir}

%clean
rm -rf %{buildroot}

%post
echo "Nagios checks installed:"
echo "    /usr/bin/check-page-fragmentation"
echo "    /usr/bin/check-rabbitmq-exchange"

%files
%defattr(-,root,root,-)
%{_bindir}/check-page-fragmentation
%{_bindir}/check-rabbitmq-exchange
