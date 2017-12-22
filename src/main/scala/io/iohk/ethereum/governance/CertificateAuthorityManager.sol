pragma solidity ^0.4.11;

contract CertificateAuthorityManager {

	event CAQueriedFor(address sender, uint slotNumber);

	address a1 = 0x00b15396fbf95ced0bc4a961239ff6350cca2b3203;

	function isCertificateAuthorityFor(address sender, uint slotNumber) constant returns(bool) {
		CAQueriedFor(sender, slotNumber);
		// TODO: Implement.
		return a1 == sender;
	}
}
