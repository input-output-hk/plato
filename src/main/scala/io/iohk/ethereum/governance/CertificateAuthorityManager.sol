pragma solidity ^0.4.11;

contract CertificateAuthorityManager {

	address[] internal certificateAuthorities =  [0x00b15396fbf95ced0bc4a961239ff6350cca2b3203];

	function getCertificateAuthorities() public constant returns(address[]) {
		return certificateAuthorities;
	}

	function isElectedCAForNextBlock(address ca, uint slotNumber) public constant returns(bool) {
		uint electedCAIndex = slotNumber % certificateAuthorities.length;	
		return certificateAuthorities[electedCAIndex] == ca;
	}

	function addCA(address newCA) public returns(address[]) {
		bool hasAuthority = false;
		bool isNew = true;
		for (uint it = 0; it < certificateAuthorities.length; it++) {
    	    if (certificateAuthorities[it] == msg.sender) {
    	       	hasAuthority = true;
    	    }
			if (certificateAuthorities[it] == newCA) {
				isNew = false;
			}
    	}
		if (hasAuthority && isNew) {
			certificateAuthorities.push(newCA);
		}
		return certificateAuthorities;
	}
	
	function removeCA(address ca) public returns(address[]) {
		bool hasAuthority = false;
		bool isCA = false;
		uint removeIndex = 0;
		for (uint it = 0; it < certificateAuthorities.length; it++) {
    	    if (certificateAuthorities[it] == msg.sender) {
    	       	hasAuthority = true;
    	    }
			if (certificateAuthorities[it] == ca) {
				isCA = true;
				removeIndex = it;
			}
    	}
		if (hasAuthority && isCA) {
			certificateAuthorities = remove(certificateAuthorities, removeIndex);
		}
		return certificateAuthorities;
	}

	function remove(address[] accounts, uint removeIndex) internal pure returns(address[]) {	
        if (removeIndex >= accounts.length) return;
		address[] memory arrayNew = new address[](accounts.length - 1);
        for (uint itBefore = 0; itBefore < removeIndex; itBefore++) {
            arrayNew[itBefore] = accounts[itBefore];
        }
		for (uint itAfter = removeIndex; itAfter < arrayNew.length; itAfter++) {
            arrayNew[itAfter] = accounts[itAfter];
        }
        return arrayNew;
    }
}
