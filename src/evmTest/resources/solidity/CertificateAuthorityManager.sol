pragma solidity ^0.4.11;

contract CertificateAuthorityManager {
	/* warning: The first value MUST be 'NoVote' because is the default value for the enums variables */
	enum VoteType { NoVote, VoteForAdd, VoteForDelete}

	// Contract State
	address[] internal certificateAuthorities;
	mapping(address => mapping(address => VoteType)) internal caVotes;
	uint internal consensusApprovalPercentage;

	// Events
	event NewVote (
		address fromCa,
        address toCandidate,
		string voteType
	);
	event InvalidVote (
		address from,
		address toCandidate,
		string voteType
	);
	event NewCertificateAuthority(address ca);
	event DeletedCertificateAuthority(address ca);

	// Constructor
    function CertificateAuthorityManager(address initialCertificateAuthority, uint _consensusApprovalPercentage) public {
		if (_consensusApprovalPercentage > 100) throw;
		certificateAuthorities = [initialCertificateAuthority];
		consensusApprovalPercentage = _consensusApprovalPercentage;
    }

	// Methods
	function getCertificateAuthorities() public constant returns(address[]) {
		return certificateAuthorities;
	}

	function getConsensusApprovalPercentage() public constant returns(uint) {
		return consensusApprovalPercentage;
	}

	/*
	    Note: Everytime a certificate authority is added or removed,
		the current implementation of the round robin chooses the next authority based on
		who had been be elected if all authorities had been part of the group since the beginning.
	*/
	function isElectedCertificateAuthorityForSlot(address ca, uint slotNumber) public constant returns(bool) {
		uint electedCAIndex = slotNumber % certificateAuthorities.length;	
		return certificateAuthorities[electedCAIndex] == ca;
	}
	
	/* 
		Note: Be careful, If there are many certificate authorities, 
		the operations will be too expensive and will became impossible to execute.
	*/
	function voteForAddCA(address caCandidate) public {
		if (isValidAddRequest(caCandidate, msg.sender, certificateAuthorities)) {
			caVotes[msg.sender][caCandidate] = VoteType.VoteForAdd;
			NewVote(msg.sender, caCandidate, "VoteForAdd");
			uint votes = countVotes(caCandidate, certificateAuthorities, VoteType.VoteForAdd);
			if (consensusExist(votes, certificateAuthorities.length)) {
				cleanVotes(caCandidate, certificateAuthorities, VoteType.VoteForAdd);
				certificateAuthorities.push(caCandidate); // Add new CA
				NewCertificateAuthority(caCandidate);
			}
		} else {
			InvalidVote(msg.sender, caCandidate, "VoteForAdd");
		}
	}
	
	/* 
		Note: Be careful, if you are the only authority in the list and votes for remove yourself,
		the blockchain will be halted forever.
	*/
	function voteForRemoveCA(address caCandidate) public {
		var (isDeletable, removeIndex) = isValidDeleteRequest(caCandidate, msg.sender, certificateAuthorities);
		if (isDeletable) {
			caVotes[msg.sender][caCandidate] = VoteType.VoteForDelete;
			NewVote(msg.sender, caCandidate, "VoteForDelete");
			uint votes = countVotes(caCandidate, certificateAuthorities, VoteType.VoteForDelete);
			if (consensusExist(votes, certificateAuthorities.length)) {
				cleanVotes(caCandidate, certificateAuthorities, VoteType.VoteForDelete);
				certificateAuthorities = remove(certificateAuthorities, removeIndex); // Update list
				DeletedCertificateAuthority(caCandidate);
			}
		} else {
			InvalidVote(msg.sender, caCandidate, "VoteForDelete");
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
		return (hasAuthority && isNew);
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

	function consensusExist(uint votes, uint voters) internal returns(bool) {
		/* note: Using this equivalent formula we avoid the uses of float variables */
		return (votes * 100 >= voters * consensusApprovalPercentage);
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

	// Note: Be careful, this method Must update the caVotes from the contract state.
	function cleanVotes(address caCandidate, address[] authorities, VoteType forVoteType) internal {
		for (uint it = 0; it < authorities.length; it++) {
			if (caVotes[authorities[it]][caCandidate] == forVoteType) {
				caVotes[authorities[it]][caCandidate] = VoteType.NoVote;
			}
		}	
	}
}
