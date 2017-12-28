pragma solidity ^0.4.11;

contract CertificateAuthorityManager {

	address[] internal certificateAuthorities;

    function CertificateAuthorityManager(address initialCertificateAuthority) public {
        certificateAuthorities = [initialCertificateAuthority];
    }

	function getCertificateAuthorities() public constant returns(address[]) {
		return certificateAuthorities;
	}

	function isElectedCAForNextBlock(address ca, uint slotNumber) public constant returns(bool) {
		uint electedCAIndex = slotNumber % certificateAuthorities.length;	
		return certificateAuthorities[electedCAIndex] == ca;
	}

	function addCA(address newCA) public {
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
	}
	
	function removeCA(address ca) public {
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
	}

	function remove(address[] accounts, uint removeIndex) internal returns(address[]) {	
        if (removeIndex >= accounts.length) return accounts;
		address[] memory arrayNew = new address[](accounts.length - 1);
        for (uint itBefore = 0; itBefore < removeIndex; itBefore++) {
            arrayNew[itBefore] = accounts[itBefore];
        }
		for (uint itAfter = removeIndex; itAfter < arrayNew.length; itAfter++) {
            arrayNew[itAfter] = accounts[itAfter + 1];
        }
        return arrayNew;
    }
}
