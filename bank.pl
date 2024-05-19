:- dynamic bank/2.
:- dynamic client/5.
:- dynamic account/5.

% Facts about banks
% bank(bankID, bankName)
bank(1, 'Bank A').
bank(2, 'Bank B').



% Facts about accounts
% account(accountNumber, bankID, IBAN, clientID, balance)
account(1001, 1, 'TR10010001', 1, 10000).
account(1003, 1, 'TR10030001', 3, 10000).
account(1002, 2, 'TR10020001', 2, 10000).

% Facts about clients
% client(clientID, NationalID, Name, Surname, Gender)
client(1, '11111111111', 'John', 'Doe', 'M').
client(2, '22222222222', 'Jane', 'Doe', 'F').
client(3, '33333333333', 'Alice', 'Smith', 'F').

% Function to create a new bank
% createBank(BankID, BankName)
createBank(BankID, BankName) :-
    assertz(bank(BankID, BankName)).

% Function to create a new client
% createClient(clientID, NationalID, Name, Surname, Gender)
createClient(clientID, NationalID, Name, Surname, Gender) :-
    assertz(client(clientID, NationalID, Name, Surname, Gender)).

% Function to create a new account
% createAccount(AccountNumber, BankID, IBAN, ClientID, Balance)
createAccount(AccountNumber, BankID, IBAN, ClientID, Balance) :-
    assertz(account(AccountNumber, BankID, IBAN, ClientID, Balance)).


% Rule for EFT
eft(SenderIban, ReceiverIban, Amount) :-
    account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance),
    account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance),
    (SenderBank \= ReceiverBank, Amount =< 1000 -> Fee = 5 ; Fee is Amount * 0.03),
    SenderBalance >= Amount + Fee,
    NewSenderBalance is SenderBalance - Amount - Fee,
    NewReceiverBalance is ReceiverBalance + Amount,
    retract(account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance)),
    assertz(account(SenderAccount, SenderBank, SenderIban, SenderClient, NewSenderBalance)),
    retract(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance)),
    assertz(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, NewReceiverBalance)),
    write('Sender Balance:'),
    write(NewSenderBalance), nl,
    write('Receiver Balance:'),
    write(NewReceiverBalance).

% Rule for transfer
transfer(SenderAccountNumber, ReceiverName, ReceiverSurname, Amount) :-
    account(SenderAccountNumber, _, _, _, SenderBalance),
    client(ReceiverClient, _, ReceiverName, ReceiverSurname, _),
    account(ReceiverAccount, _, _, ReceiverClient, ReceiverBalance),
    SenderBalance >= Amount,
    NewSenderBalance is SenderBalance - Amount,
    NewReceiverBalance is ReceiverBalance + Amount,
    retract(account(SenderAccountNumber, SenderBank, SenderIban, SenderClient, SenderBalance)),
    assert(account(SenderAccountNumber, SenderBank, SenderIban, SenderClient, NewSenderBalance)),
    retract(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance)),
    assert(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, NewReceiverBalance)),
    write('Transfer transaction is successful.').

% Rule for balances
balance(ClientID) :-
    account(_, _, _, ClientID, Balance),
    write('Balance:'),
    write(Balance).

% Rule for account information
accountInfo(ClientID) :-
    account(AccountNumber, BankID, IBAN, ClientID, _),
    bank(BankID, BankName),
    client(ClientID, NationalID, Name , Surname, Gender),
    write('Account Number:'),
    write(AccountNumber), nl,
    write('Bank Name:'),
    write(BankName), nl,
    write('IBAN:'),
    write(IBAN), nl,
    write('Client Name:'),
    write(Name), 
    write(Surname), nl,
    write('National ID:'),
    write(NationalID), nl,
    write('Gender:'),
    write(Gender).

% Rule for account information for all accounts
accountInfo('ALL') :-
    findall(ClientID, account(_, _, _, ClientID, _), ClientIDs),
    listAccounts(ClientIDs).

% Helper predicate to list all accounts
listAccounts([]).
listAccounts([ClientID|Rest]) :-
    accountInfo(ClientID),
    nl,
    listAccounts(Rest).

% Rule for bank information for all banks
bankInfo('ALL') :-
    findall(BankID, bank(BankID, _), BankIDs),
    listBanks(BankIDs).

% Helper predicate to list all banks
listBanks([]).
listBanks([BankID|Rest]) :-
    bankInfo(BankID),
    nl,
    listBanks(Rest).

% Rule for bank information for a specific bank
bankInfo(BankID) :-
    bank(BankID, BankName),
    write('Bank ID:'),
    write(BankID), nl,
    write('Bank Name:'),
    write(BankName).
    
