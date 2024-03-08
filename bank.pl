:- dynamic account/5.
% Facts about banks
bank(1, 'Bank A').
bank(2, 'Bank B').

% Facts about accounts
account(1001, 1, 'TR10010001', 1, 5000).
account(1002, 2, 'TR10020001', 2, 8000).

% Facts about clients
client(1, '11111111111', 'John', 'Doe', 'M').
client(2, '22222222222', 'Jane', 'Doe', 'F').

% Rule for EFT
eft(SenderIban, ReceiverIban, Amount) :-
    account(SenderAccount, _, SenderIban, _, SenderBalance),
    account(ReceiverAccount, _, ReceiverIban, _, _),
    SenderBalance >= Amount,
    NewSenderBalance is SenderBalance - Amount,
    retract(account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance)),
    assert(account(SenderAccount, SenderBank, SenderIban, SenderClient, NewSenderBalance)),
    write('EFT transaction is successful.').

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