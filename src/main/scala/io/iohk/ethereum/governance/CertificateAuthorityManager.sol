pragma solidity ^0.4.11;

contract CertificateAuthorityManager {

	event CAQueriedFor(address sender, uint slotNumber);

	address a1 = 0x00edde8656c35fcb7126c61fc6e2673734425a72bf;

	function isCertificateAuthorityFor(address sender, uint slotNumber) constant returns(bool) {
		CAQueriedFor(sender, slotNumber);
		// TODO: Implement.
		return a1 == sender;
	}
}
