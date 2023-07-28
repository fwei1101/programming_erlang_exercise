-module(mp3_sync).
-export([find_sync/2]).

%%% MPEG Frame Header
%%% AAAAAAAA AAABBCCD EEEEFFGH IIJJKLMM
%%% Sign    Length(bits)    Position(bits)  Description
%%% A       11              (31-21)         Frame Sync (all bits must be set)
%%% B       2               (20,19)         MPEG Audio version ID
%%% C       2               (18,17)         Layer description
%%% D       1               (16)            Protection bit
%%% E       4               (15,12)         Bitrate index
%%% F       2               (11,10)         Sampling rate frequency index
%%% G       1               (9)             Padding bit
%%% H       1               (8)             Private bit
%%% I       2               (7,6)           Channel Mode
%%% J       2               (5,4)           Mode extension
%%% K       1               (3)             Copyright
%%% L       1               (2)             Original
%%% M       2               (1,0)           Emphasis
%%% http://www.mp3-tech.org/programmer/frame_header.html

-define(BITRATE_INDEX_V1L1, {free,32,64,96,128,160,192,224,256,288,320,352,384,416,448}).
-define(BITRATE_INDEX_V1L2, {free,32,48,56,64,80,96,112,128,160,192,224,256,320,384}).
-define(BITRATE_INDEX_V1L3, {free,32,40,48,56,64,80,96,112,128,160,192,224,256,320}).
-define(BITRATE_INDEX_V2L1, {free,32,48,56,64,80,96,112,128,144,160,176,192,224,256}).
-define(BITRATE_INDEX_V2L2, {free,8,16,24,32,40,48,56,64,80,96,112,128,144,160}).

find_sync(Bin, N) ->
    case is_header(N, Bin) of
        {ok, Len1, _} ->
            case is_header(N + Len1, Bin) of
                {ok, Len2, _} ->
                    case is_header(N + Len1 + Len2, Bin) of
                        {ok, _, _} ->
                            {ok, N};
                        error ->
                            find_sync(Bin, N + 1)
                    end;
                error ->
                    find_sync(Bin, N + 1)
            end;
        error ->
            find_sync(Bin, N + 1)
    end.

is_header(N, Bin) ->
    unpack_header(get_word(N,Bin)).

get_word(N, Bin) ->
    {_, <<C:4/binary, _/binary>>} = split_binary(Bin, N),
    C.

unpack_header(X) ->
    try decode_header(X)
    catch
        _:_ -> error
    end.

decode_header(<<2#11111111111:11, B:2, C:2, _D:1, E:4, F:2, G:1, Bits:9>>) ->
    Vsn = case B of
        0 -> {2,5};
        1 -> exit(badVsn);
        2 -> 2;
        3 -> 1
    end,
    Layer = case C of
        0 -> exit(badLayer);
        1 -> 3;
        2 -> 2;
        3 -> 1
    end,
    % Protection = D,
    Bitrate = bitrate(Vsn, Layer, E) * 1000,
    SampleRate = samplerate(Vsn, F),
    Padding = G,
    FrameLength = framelength(Layer, Bitrate, SampleRate, Padding),
    if FrameLength < 21 ->
        exit(frameSize);
    true ->
        {ok, FrameLength, {Layer, Bitrate, SampleRate, Vsn, Bits}}
    end;
decode_header(_) ->
    exit(badHeader).

bitrate(_,_,15) -> exit(badBitrate);
bitrate(1,1,E) ->		      
    element(E+1, ?BITRATE_INDEX_V1L1);
bitrate(1,2,E) ->
    element(E+1, ?BITRATE_INDEX_V1L2);
bitrate(1,3,E) ->
    element(E+1, ?BITRATE_INDEX_V1L3);
bitrate(2,1,E) ->
    element(E+1, ?BITRATE_INDEX_V2L1);
bitrate(2,2,E) ->
    element(E+1, ?BITRATE_INDEX_V2L2);
bitrate(2,3,E) -> bitrate(2,2,E);
bitrate({2,5}, L, E) -> bitrate(2, L, E).

%% samplerate Vsn F
samplerate(1, 0) -> 44100;
samplerate(1, 1) -> 48000;
samplerate(1, 2) -> 32000;
samplerate(2, 0) -> 22050;
samplerate(2, 1) -> 24000;
samplerate(2, 2) -> 16000;
samplerate({2,5}, 0) -> 11025;
samplerate({2,5}, 1) -> 12000;
samplerate({2,5}, 2) -> 8000.

framelength(1, BitRate, SampleRate, Padding) ->
    ((12*BitRate div SampleRate) + Padding) * 4;
framelength(_, BitRate, SampleRate, Padding) ->
    (144 * BitRate div SampleRate) + Padding.