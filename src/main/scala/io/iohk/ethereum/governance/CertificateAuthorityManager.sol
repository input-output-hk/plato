pragma solidity ^0.4.11;

contract CertificateAuthorityManager {
	enum VoteType { VoteForAdd, VoteForDelete, NoVote }

	// Contract State
	address[] internal certificateAuthorities;
	mapping(address => mapping(address => VoteType)) internal caVotes;

	// Events
	event NewVote (
		address fromCa,
        address toCandidate,
		string voteType
	);
	event NewCetificateAuthority(address ca);
	event DeletedCetificateAuthority(address ca);

	// Constructor
    function CertificateAuthorityManager(address initialCertificateAuthority) public {
        certificateAuthorities = [initialCertificateAuthority];
    }

	// Methods
	function getCertificateAuthorities() public constant returns(address[]) {
		return certificateAuthorities;
	}

	function isElectedCAForNextBlock(address ca, uint slotNumber) public constant returns(bool) {
		uint electedCAIndex = slotNumber % certificateAuthorities.length;	
		return certificateAuthorities[electedCAIndex] == ca;
	}

	function addCA(address caCandidate) public {
		if (isValidAddRequest(caCandidate, msg.sender, certificateAuthorities)) {
			caVotes[msg.sender][caCandidate] = VoteType.VoteForAdd;
			NewVote(msg.sender, caCandidate, "VoteForAdd");
			// Count votes
			uint votes = countVotes(caCandidate, certificateAuthorities, VoteType.VoteForAdd);
			// Check if there is courum
			if (votes * 3 >= certificateAuthorities.length * 2) { // obs: request 2/3 of the total
				cleanVotes(caCandidate, certificateAuthorities, VoteType.VoteForAdd);
				certificateAuthorities.push(caCandidate); // Add new CA
				NewCetificateAuthority(caCandidate);
			}
		}
	}
	
	function removeCA(address caCandidate) public {
		var (isDeletable, removeIndex) = isValidDeleteRequest(caCandidate, msg.sender, certificateAuthorities);
		if (isDeletable) {
			caVotes[msg.sender][caCandidate] = VoteType.VoteForDelete;
			NewVote(msg.sender, caCandidate, "VoteForDelete");
			uint votes = countVotes(caCandidate, certificateAuthorities, VoteType.VoteForDelete);
			// Check if there is courum
			if (votes * 3 >= certificateAuthorities.length * 2) { // obs: request 2/3 of the total
				cleanVotes(caCandidate, certificateAuthorities, VoteType.VoteForDelete);
				certificateAuthorities = remove(certificateAuthorities, removeIndex); // Update list
				DeletedCetificateAuthority(caCandidate);
			}
		}
	}

	// Internals
	function isValidAddRequest(address caCandidate, address sender, address[] authorities) internal returns(bool) {
		bool hasAuthority = false;
		bool isNew = true;
		for (uint it = 0; it < authorities.length; it++) {
    	    if (authorities[it] == sender) {
    	       	hasAuthority = true;
    	    }
			if (authorities[it] == caCandidate) {
				isNew = false;
			}
    	}
		return hasAuthority && isNew;
	}

	function isValidDeleteRequest(address caCandidate, address sender, address[] authorities) internal returns(bool, uint) {
		bool hasAuthority = false;
		bool isCA = false;
		uint removeIndex = 0;
		for (uint it = 0; it < authorities.length; it++) {
    	    if (authorities[it] == sender) {
    	       	hasAuthority = true;
    	    }
			if (authorities[it] == caCandidate) {
				isCA = true;
				removeIndex = it;
			}
    	}
		return (hasAuthority && isCA, removeIndex);
	}

	function remove(address[] accounts, uint removeIndex) internal returns(address[]) {	
        if (removeIndex >= accounts.length) return accounts;
		address[] memory arrayNew = new address[](accounts.length - 1);
		uint it;
        for (it = 0; it < removeIndex; it++) {
            arrayNew[it] = accounts[it];
        }
		for (it = removeIndex; it < arrayNew.length; it++) {
            arrayNew[it] = accounts[it + 1];
        }
        return arrayNew;
    }

	function countVotes(address caCandidate, address[] authorities, VoteType voteType) internal returns(uint) {
		uint votes = 0;
		for (uint it = 0; it < authorities.length; it++) {
			var caVoteForCandidate = caVotes[authorities[it]][caCandidate];
			if (caVoteForCandidate == voteType) {
				votes++;
			}
		}
		return votes;
	}

	// Note: Be carful, this method Must update the caVotes from the contract state.
	function cleanVotes(address caCandidate, address[] authorities, VoteType forVoteType) internal {
		for (uint it = 0; it < authorities.length; it++) {
			if (caVotes[authorities[it]][caCandidate] == forVoteType) {
				caVotes[authorities[it]][caCandidate] = VoteType.NoVote;
			}
		}	
	}
}
