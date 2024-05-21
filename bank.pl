% Dinamik predikat tanımları
:- dynamic(account/5).
:- dynamic(bank/2).
:- dynamic(client/5).

% Bankalar hakkında bilgiler
% bank(bankID, bankName)
bank(34, 'Vakifbank').
bank(35, 'Diger Bankalar').

% Hesaplar hakkında bilgiler
% account(accountNumber, bankID, IBAN, clientID, balance)
account(1001, 34, 'TR10010001', 1, 10000).
account(1003, 34, 'TR10030001', 3, 10000).
account(1002, 35, 'TR10020001', 2, 10000).

% Müşteriler hakkında bilgiler
% client(clientID, Password, Name, Surname, Gender)
client(1, 123, 'John', 'Doe', 1).
client(2, 234, 'Jane', 'Doe', 2).
client(3, 345, 'Alice', 'Smith', 3).

% Ana program girişi
:- initialization(login(0)).

% Giriş fonksiyonu
login(AttemptCount) :-
    (AttemptCount >= 5 ->
        write('Cok fazla yanlis giris yaptiniz. Tekrar denemek icin programi yeniden baslatin.'), nl;
        write('Vakifbank\'a hos geldiniz! ...'), nl,
        write('Musteri ID\'nizi girin: '), read(ClientID),
        (clientExists(ClientID) ->
            write('Sifrenizi girin: '), read(Password),
            (verifyPassword(ClientID, Password) ->
                write('Giris basarili!'), nl, mainMenu(ClientID);
                write('Yanlis sifre girdiniz, lutfen tekrar deneyin.'), nl,
                NewAttemptCount is AttemptCount + 1,
                login(NewAttemptCount)
            );
            write('Kimlik bilginizi kontrol ediniz, lutfen tekrar deneyin.'), nl,
            NewAttemptCount is AttemptCount + 1,
            login(NewAttemptCount)
        )
    ).

% Kullanıcı kimlik doğrulama (kimlik kontrolü)
clientExists(ClientID) :-
    client(ClientID, _, _, _, _).

% Kullanıcı kimlik doğrulama (şifre kontrolü)
verifyPassword(ClientID, Password) :-
    client(ClientID, Password, _, _, _).

% Ana menu
mainMenu(ClientID) :-
    client(ClientID, _, Name, _, Gender),
    (Gender == 1 ->
        Greeting = 'Bey';
        Greeting = 'Hanım'),
    format('Iyi Gunler, ~w ~w!~n', [Name, Greeting]),
    write('1. Tüm banka hesaplarını listele'), nl,
    write('2. EFT islemleri'), nl,
    write('3. Havale islemleri'), nl,
    write('4. Fast islemleri'), nl,
    write('5. Cikis yap'), nl,
    write('Seçiminiz: '), read(Choice),
    handleMenuChoice(Choice, ClientID).

% Menu seçimlerini yönetme
handleMenuChoice(1, ClientID) :- listAllAccounts(ClientID).
handleMenuChoice(2, ClientID) :- eftMenu(ClientID).
handleMenuChoice(3, ClientID) :- transferMenu(ClientID).
handleMenuChoice(4, ClientID) :- fastMenu(ClientID).
handleMenuChoice(5, _) :- write('Goodbye!'), nl.
handleMenuChoice(_, ClientID) :-
    write('Invalid choice, please try again.'), nl,
    mainMenu(ClientID).

% Placeholder predikatar
% Tüm banka hesaplarını listele
listAllAccounts(ClientID) :-
    findall((AccountNumber, BankName, IBAN, Balance), 
            (account(AccountNumber, BankID, IBAN, ClientID, Balance), bank(BankID, BankName)), Accounts),
    length(Accounts, AccountCount),
    (AccountCount =:= 0 -> 
        write('No accounts found.'), nl;
        printAccounts(Accounts),
        postMenu(ClientID)).

% Hesapları yazdırma
printAccounts([]).
printAccounts([(AccountNumber, BankName, IBAN, Balance)|Rest]) :-
    format('Account Number: ~w~n', [AccountNumber]),
    write('Bank: '), write(BankName), nl,
    write('IBAN: '), write(IBAN), nl,
    write('Balance: '), write(Balance), nl, nl,
    printAccounts(Rest).

% Menu sonrası işlemler
postMenu(ClientID) :-
    write('Başka bir işlem yapmak ister misiniz?'), nl,
    mainMenu(ClientID). % Ana menüye geri dönme seçeneği ekledik

% Hesap seçme
selectAccount(ClientID, SelectedAccount) :-
    findall((AccountNumber, BankName, IBAN, Balance), 
            (account(AccountNumber, BankID, IBAN, ClientID, Balance), bank(BankID, BankName)), Accounts),
    printAccounts(Accounts),
    write('Select the account number: '), read(SelectedAccount),
    (member((SelectedAccount, _, _, _), Accounts) ->
        true;
        write('Invalid account number. Please try again.'), nl,
        selectAccount(ClientID, SelectedAccount)).

% EFT Menüsü
eftMenu(ClientID) :-
    write('EFT islemleri'), nl,
    selectAccount(ClientID, SenderAccount),
    account(SenderAccount, _, SenderIban, _, _),
    write('Alıcı hesap IBAN\'ını girin: '), read(ReceiverIban),
    write('Gonderilecek miktarı girin: '), read(Amount),
    (eft(SenderIban, ReceiverIban, Amount) ->
        write('EFT islemi basarili!'), nl,
        postMenu(ClientID);
        write('EFT islemi basarisiz. Bakiye yetersiz veya hatali bilgi.'), nl,
        postMenu(ClientID)
    ).

% IBAN kontrolü
valid_iban(Iban) :-
    account(_, _, Iban, _, _).

% EFT Kuralları
eft(SenderIban, ReceiverIban, Amount) :-
    valid_iban(SenderIban),
    valid_iban(ReceiverIban),
    account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance),
    account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance),
    (SenderBank \= ReceiverBank, Amount =< 1000 -> Fee = 5 ; Fee is Amount * 0.03),
    TotalAmount is Amount + Fee,
    SenderBalance >= TotalAmount,
    NewSenderBalance is SenderBalance - TotalAmount,
    NewReceiverBalance is ReceiverBalance + Amount,
    retract(account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance)),
    assertz(account(SenderAccount, SenderBank, SenderIban, SenderClient, NewSenderBalance)),
    retract(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance)),
    assertz(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClient, NewReceiverBalance)),
    format('EFT basarili. Bakiye: ~w, Komisyon ucreti: ~w~n', [NewSenderBalance, Fee]).

eft(SenderIban, _, _) :-
    \+ valid_iban(SenderIban),
    write('EFT islemi basarisiz. Gecersiz gonderici IBAN.'), nl.

eft(_, ReceiverIban, _) :-
    \+ valid_iban(ReceiverIban),
    write('EFT islemi basarisiz. Gecersiz alici IBAN.'), nl.

eft(SenderIban, ReceiverIban, Amount) :-
    valid_iban(SenderIban),
    valid_iban(ReceiverIban),
    account(_, _, SenderIban, _, SenderBalance),
    (SenderBalance < Amount ->
        write('EFT islemi basarisiz. Bakiye yetersiz.'), nl).

% Transfer Menüsü
transferMenu(ClientID) :-
    write('Havale islemleri'), nl,
    selectAccount(ClientID, SenderAccount),
    account(SenderAccount, _, SenderIban, _, _),
    write('Alıcı hesap numarasını girin: '), read(ReceiverAccountNumber),
    write('Gonderilecek miktarı girin: '), read(Amount),
    (transfer(SenderIban, ReceiverAccountNumber, Amount) ->
        write('Havale islemi basarili!'), nl,
        postMenu(ClientID);
        write('Havale islemi basarisiz. Bakiye yetersiz veya hatali bilgi.'), nl,
        postMenu(ClientID)
    ).

% Transfer Kuralları
transfer(SenderIban, ReceiverAccountNumber, Amount) :-
    valid_iban(SenderIban),
    account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance),
    account(_, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance),
    (SenderBank \= ReceiverBank, Amount =< 1000 -> Fee = 5 ; Fee is Amount * 0.03),
    TotalAmount is Amount + Fee,
    SenderBalance >= TotalAmount,
    NewSenderBalance is SenderBalance - TotalAmount,
    NewReceiverBalance is ReceiverBalance + Amount,
    retract(account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance)),
    assertz(account(SenderAccount, SenderBank, SenderIban, SenderClient, NewSenderBalance)),
    retract(account(ReceiverAccountNumber, ReceiverBank, ReceiverIban, ReceiverClient, ReceiverBalance)),
    assertz(account(ReceiverAccountNumber, ReceiverBank, ReceiverIban, ReceiverClient, NewReceiverBalance)),
    format('Havale basarili. Bakiye: ~w, Komisyon ucreti: ~w~n', [NewSenderBalance, Fee]).

transfer(SenderIban, _, _) :-
    \+ valid_iban(SenderIban),
    write('Havale islemi basarisiz. Gecersiz gonderici IBAN.'), nl.

transfer(, ReceiverAccountNumber,) :-
    \+ account(ReceiverAccountNumber, _, _, _, _),
    write('Havale islemi basarisiz. Gecersiz alici hesap numarasi.'), nl.

transfer(SenderIban, ReceiverAccountNumber, Amount) :-
    valid_iban(SenderIban),
    account(ReceiverAccountNumber, _, _, _, _),
    account(_, _, SenderIban, _, SenderBalance),
    (SenderBalance < Amount ->
        write('Havale islemi basarisiz. Bakiye yetersiz.'), nl).

% FAST Menüsü
fastMenu(ClientID) :-
    write('FAST islemleri'), nl,
    selectAccount(ClientID, SenderAccount),
    account(SenderAccount, _, SenderIban, _, _),
    write('Alıcı client ID\'sini girin: '), read(ReceiverClientID),
    write('Gonderilecek miktarı girin: '), read(Amount),
    (fast(SenderIban, ReceiverClientID, Amount) ->
        write('FAST islemi basarili!'), nl,
        postMenu(ClientID);
        write('FAST islemi basarisiz. Bakiye yetersiz veya hatali bilgi.'), nl,
        postMenu(ClientID)
    ).

% FAST Kuralları
fast(SenderIban, ReceiverClientID, Amount) :-
    valid_iban(SenderIban),
    account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance),
    account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClientID, ReceiverBalance),
    Amount =< 2000,
    SenderBalance >= Amount,
    NewSenderBalance is SenderBalance - Amount,
    NewReceiverBalance is ReceiverBalance + Amount,
    retract(account(SenderAccount, SenderBank, SenderIban, SenderClient, SenderBalance)),
    assertz(account(SenderAccount, SenderBank, SenderIban, SenderClient, NewSenderBalance)),
    retract(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClientID, ReceiverBalance)),
    assertz(account(ReceiverAccount, ReceiverBank, ReceiverIban, ReceiverClientID, NewReceiverBalance)),
    format('FAST basarili. Bakiye: ~w~n', [NewSenderBalance]).

fast(SenderIban, _, _) :-
    \+ valid_iban(SenderIban),
    write('FAST islemi basarisiz. Gecersiz gonderici IBAN.'), nl.

fast(_, ReceiverClientID, _) :-
    \+ account(_, _, _, ReceiverClientID, _),
    write('FAST islemi basarisiz. Gecersiz alici client ID.'), nl.

fast(SenderIban, ReceiverClientID, Amount) :-
    valid_iban(SenderIban),
    account(_, _, _, ReceiverClientID, _),
    account(_, _, SenderIban, _, SenderBalance),
    (SenderBalance < Amount ->
        write('FAST islemi basarisiz. Bakiye yetersiz.'), nl).

fast(_, _, Amount) :-
    Amount > 2000,
    write('FAST islemi basarisiz. Maximum gonderim miktari 2000.'), nl.
