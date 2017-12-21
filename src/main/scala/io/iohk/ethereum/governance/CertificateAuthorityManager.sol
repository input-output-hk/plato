pragma solidity ^0.4.11;

contract CertificateAuthorityManager {

	event CAQueriedFor(address sender, uint slotNumber);

	function isCertificateAuthorityFor(address sender, uint slotNumber) constant returns(bool) {
		CAQueriedFor(sender, slotNumber);
		// TODO: Implement.
		return true;
	}
}
