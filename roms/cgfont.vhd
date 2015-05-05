-- generated with romgen by MikeJ
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity CG_FONT is
  port (
    CLK         : in    std_logic;
    ADDR        : in    std_logic_vector(10 downto 0);
    DATA        : out   std_logic_vector(7 downto 0)
    );
end;

architecture RTL of CG_FONT is


  type ROM_ARRAY is array(0 to 2047) of std_logic_vector(7 downto 0);
  constant ROM : ROM_ARRAY := (
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x0000
    x"00",x"00",x"30",x"30",x"00",x"00",x"00",x"00", -- 0x0008
    x"00",x"00",x"0C",x"0C",x"00",x"00",x"00",x"00", -- 0x0010
    x"00",x"00",x"3C",x"3C",x"00",x"00",x"00",x"00", -- 0x0018
    x"00",x"00",x"00",x"00",x"30",x"30",x"00",x"00", -- 0x0020
    x"00",x"00",x"30",x"30",x"30",x"30",x"00",x"00", -- 0x0028
    x"00",x"00",x"0C",x"0C",x"30",x"30",x"00",x"00", -- 0x0030
    x"00",x"00",x"3C",x"3C",x"30",x"30",x"00",x"00", -- 0x0038
    x"00",x"00",x"00",x"00",x"0C",x"0C",x"00",x"00", -- 0x0040
    x"00",x"00",x"30",x"30",x"0C",x"0C",x"00",x"00", -- 0x0048
    x"00",x"00",x"0C",x"0C",x"0C",x"0C",x"00",x"00", -- 0x0050
    x"00",x"00",x"3C",x"3C",x"0C",x"0C",x"00",x"00", -- 0x0058
    x"00",x"00",x"00",x"00",x"3C",x"3C",x"00",x"00", -- 0x0060
    x"00",x"00",x"30",x"30",x"3C",x"3C",x"00",x"00", -- 0x0068
    x"00",x"00",x"0C",x"0C",x"3C",x"3C",x"00",x"00", -- 0x0070
    x"00",x"00",x"3C",x"3C",x"3C",x"3C",x"00",x"00", -- 0x0078
    x"44",x"10",x"28",x"44",x"7C",x"44",x"44",x"00", -- 0x0080
    x"44",x"38",x"44",x"44",x"44",x"44",x"38",x"00", -- 0x0088
    x"44",x"00",x"44",x"44",x"44",x"44",x"38",x"00", -- 0x0090
    x"44",x"00",x"38",x"04",x"3C",x"44",x"3C",x"00", -- 0x0098
    x"44",x"00",x"38",x"44",x"44",x"44",x"38",x"00", -- 0x00A0
    x"44",x"00",x"44",x"44",x"44",x"4C",x"34",x"00", -- 0x00A8
    x"38",x"44",x"44",x"78",x"44",x"44",x"78",x"40", -- 0x00B0
    x"7E",x"22",x"10",x"08",x"10",x"22",x"7E",x"00", -- 0x00B8
    x"10",x"10",x"7C",x"10",x"10",x"00",x"7C",x"00", -- 0x00C0
    x"06",x"18",x"60",x"18",x"06",x"00",x"7E",x"00", -- 0x00C8
    x"60",x"18",x"06",x"18",x"60",x"00",x"7E",x"00", -- 0x00D0
    x"0F",x"08",x"08",x"C8",x"48",x"28",x"18",x"08", -- 0x00D8
    x"00",x"32",x"4C",x"00",x"32",x"4C",x"00",x"00", -- 0x00E0
    x"30",x"48",x"10",x"20",x"78",x"00",x"00",x"00", -- 0x00E8
    x"50",x"68",x"48",x"48",x"48",x"00",x"00",x"00", -- 0x00F0
    x"00",x"7E",x"00",x"7E",x"00",x"7E",x"00",x"00", -- 0x00F8
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x0100
    x"10",x"10",x"10",x"10",x"10",x"00",x"10",x"00", -- 0x0108
    x"28",x"28",x"28",x"00",x"00",x"00",x"00",x"00", -- 0x0110
    x"28",x"28",x"7C",x"28",x"7C",x"28",x"28",x"00", -- 0x0118
    x"10",x"3C",x"50",x"38",x"14",x"78",x"10",x"00", -- 0x0120
    x"60",x"64",x"08",x"10",x"20",x"4C",x"0C",x"00", -- 0x0128
    x"20",x"50",x"50",x"24",x"58",x"48",x"34",x"00", -- 0x0130
    x"10",x"10",x"10",x"00",x"00",x"00",x"00",x"00", -- 0x0138
    x"08",x"10",x"20",x"20",x"20",x"10",x"08",x"00", -- 0x0140
    x"20",x"10",x"08",x"08",x"08",x"10",x"20",x"00", -- 0x0148
    x"10",x"54",x"38",x"10",x"38",x"54",x"10",x"00", -- 0x0150
    x"00",x"10",x"10",x"7C",x"10",x"10",x"00",x"00", -- 0x0158
    x"00",x"00",x"00",x"00",x"00",x"10",x"10",x"20", -- 0x0160
    x"00",x"00",x"00",x"7C",x"00",x"00",x"00",x"00", -- 0x0168
    x"00",x"00",x"00",x"00",x"00",x"00",x"10",x"00", -- 0x0170
    x"00",x"04",x"08",x"10",x"20",x"40",x"00",x"00", -- 0x0178
    x"38",x"44",x"4C",x"54",x"64",x"44",x"38",x"00", -- 0x0180
    x"10",x"30",x"10",x"10",x"10",x"10",x"10",x"00", -- 0x0188
    x"38",x"44",x"04",x"38",x"40",x"40",x"7C",x"00", -- 0x0190
    x"7C",x"04",x"08",x"18",x"04",x"44",x"38",x"00", -- 0x0198
    x"08",x"18",x"28",x"48",x"7C",x"08",x"08",x"00", -- 0x01A0
    x"7C",x"40",x"78",x"04",x"04",x"44",x"38",x"00", -- 0x01A8
    x"1C",x"20",x"40",x"78",x"44",x"44",x"38",x"00", -- 0x01B0
    x"7C",x"04",x"04",x"08",x"10",x"20",x"40",x"00", -- 0x01B8
    x"38",x"44",x"44",x"38",x"44",x"44",x"38",x"00", -- 0x01C0
    x"38",x"44",x"44",x"3C",x"04",x"08",x"70",x"00", -- 0x01C8
    x"00",x"00",x"00",x"10",x"00",x"10",x"00",x"00", -- 0x01D0
    x"00",x"00",x"00",x"10",x"00",x"10",x"10",x"20", -- 0x01D8
    x"08",x"10",x"20",x"40",x"20",x"10",x"08",x"00", -- 0x01E0
    x"00",x"00",x"7C",x"00",x"7C",x"00",x"00",x"00", -- 0x01E8
    x"20",x"10",x"08",x"04",x"08",x"10",x"20",x"00", -- 0x01F0
    x"38",x"44",x"04",x"08",x"10",x"00",x"10",x"00", -- 0x01F8
    x"38",x"44",x"54",x"5C",x"58",x"40",x"3C",x"00", -- 0x0200
    x"10",x"28",x"44",x"44",x"7C",x"44",x"44",x"00", -- 0x0208
    x"78",x"44",x"44",x"78",x"44",x"44",x"78",x"00", -- 0x0210
    x"38",x"44",x"40",x"40",x"40",x"44",x"38",x"00", -- 0x0218
    x"78",x"44",x"44",x"44",x"44",x"44",x"78",x"00", -- 0x0220
    x"7C",x"40",x"40",x"70",x"40",x"40",x"7C",x"00", -- 0x0228
    x"7C",x"40",x"40",x"70",x"40",x"40",x"40",x"00", -- 0x0230
    x"38",x"44",x"40",x"5C",x"44",x"44",x"38",x"00", -- 0x0238
    x"44",x"44",x"44",x"7C",x"44",x"44",x"44",x"00", -- 0x0240
    x"38",x"10",x"10",x"10",x"10",x"10",x"38",x"00", -- 0x0248
    x"7C",x"04",x"04",x"04",x"04",x"44",x"38",x"00", -- 0x0250
    x"44",x"48",x"50",x"60",x"50",x"48",x"44",x"00", -- 0x0258
    x"40",x"40",x"40",x"40",x"40",x"40",x"7C",x"00", -- 0x0260
    x"44",x"6C",x"54",x"54",x"44",x"44",x"44",x"00", -- 0x0268
    x"44",x"64",x"54",x"4C",x"44",x"44",x"44",x"00", -- 0x0270
    x"38",x"44",x"44",x"44",x"44",x"44",x"38",x"00", -- 0x0278
    x"78",x"44",x"44",x"78",x"40",x"40",x"40",x"00", -- 0x0280
    x"38",x"44",x"44",x"44",x"54",x"48",x"34",x"00", -- 0x0288
    x"78",x"44",x"44",x"78",x"50",x"48",x"44",x"00", -- 0x0290
    x"38",x"44",x"40",x"38",x"04",x"44",x"38",x"00", -- 0x0298
    x"7C",x"54",x"10",x"10",x"10",x"10",x"10",x"00", -- 0x02A0
    x"44",x"44",x"44",x"44",x"44",x"44",x"38",x"00", -- 0x02A8
    x"44",x"44",x"44",x"28",x"28",x"10",x"10",x"00", -- 0x02B0
    x"44",x"44",x"44",x"54",x"54",x"54",x"28",x"00", -- 0x02B8
    x"44",x"44",x"28",x"10",x"28",x"44",x"44",x"00", -- 0x02C0
    x"44",x"44",x"28",x"10",x"10",x"10",x"10",x"00", -- 0x02C8
    x"7C",x"44",x"08",x"10",x"20",x"44",x"7C",x"00", -- 0x02D0
    x"78",x"60",x"60",x"60",x"60",x"60",x"78",x"00", -- 0x02D8
    x"00",x"40",x"20",x"10",x"08",x"04",x"00",x"00", -- 0x02E0
    x"3C",x"0C",x"0C",x"0C",x"0C",x"0C",x"3C",x"00", -- 0x02E8
    x"10",x"28",x"44",x"00",x"00",x"00",x"00",x"00", -- 0x02F0
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"7C", -- 0x02F8
    x"20",x"10",x"08",x"00",x"00",x"00",x"00",x"00", -- 0x0300
    x"00",x"00",x"38",x"04",x"3C",x"44",x"3C",x"00", -- 0x0308
    x"40",x"40",x"78",x"44",x"44",x"44",x"78",x"00", -- 0x0310
    x"00",x"00",x"38",x"44",x"40",x"44",x"38",x"00", -- 0x0318
    x"04",x"04",x"3C",x"44",x"44",x"44",x"3C",x"00", -- 0x0320
    x"00",x"00",x"38",x"44",x"7C",x"40",x"38",x"00", -- 0x0328
    x"18",x"20",x"70",x"20",x"20",x"20",x"20",x"20", -- 0x0330
    x"00",x"00",x"3C",x"44",x"44",x"3C",x"04",x"38", -- 0x0338
    x"40",x"40",x"78",x"44",x"44",x"44",x"44",x"00", -- 0x0340
    x"10",x"00",x"30",x"10",x"10",x"10",x"38",x"00", -- 0x0348
    x"04",x"00",x"04",x"04",x"04",x"04",x"24",x"18", -- 0x0350
    x"40",x"40",x"48",x"50",x"60",x"50",x"48",x"00", -- 0x0358
    x"30",x"10",x"10",x"10",x"10",x"10",x"38",x"00", -- 0x0360
    x"00",x"00",x"68",x"54",x"54",x"54",x"54",x"00", -- 0x0368
    x"00",x"00",x"78",x"44",x"44",x"44",x"44",x"00", -- 0x0370
    x"00",x"00",x"38",x"44",x"44",x"44",x"38",x"00", -- 0x0378
    x"00",x"00",x"78",x"44",x"44",x"78",x"40",x"40", -- 0x0380
    x"00",x"00",x"3C",x"44",x"44",x"3C",x"04",x"04", -- 0x0388
    x"00",x"00",x"58",x"60",x"40",x"40",x"40",x"00", -- 0x0390
    x"00",x"00",x"3C",x"40",x"38",x"04",x"78",x"00", -- 0x0398
    x"20",x"20",x"7C",x"20",x"20",x"20",x"1C",x"00", -- 0x03A0
    x"00",x"00",x"44",x"44",x"44",x"4C",x"34",x"00", -- 0x03A8
    x"00",x"00",x"44",x"44",x"44",x"28",x"10",x"00", -- 0x03B0
    x"00",x"00",x"44",x"44",x"54",x"54",x"28",x"00", -- 0x03B8
    x"00",x"00",x"44",x"28",x"10",x"28",x"44",x"00", -- 0x03C0
    x"00",x"00",x"44",x"44",x"44",x"3C",x"04",x"38", -- 0x03C8
    x"00",x"00",x"7C",x"08",x"10",x"20",x"7C",x"00", -- 0x03D0
    x"18",x"20",x"20",x"40",x"20",x"20",x"18",x"00", -- 0x03D8
    x"10",x"10",x"10",x"00",x"10",x"10",x"10",x"00", -- 0x03E0
    x"30",x"08",x"08",x"04",x"08",x"08",x"30",x"00", -- 0x03E8
    x"32",x"4C",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x03F0
    x"FF",x"FF",x"FF",x"FF",x"FF",x"FF",x"FF",x"FF", -- 0x03F8
    x"FF",x"FF",x"FF",x"07",x"07",x"07",x"07",x"07", -- 0x0400
    x"FF",x"FF",x"FF",x"E0",x"E0",x"E0",x"E0",x"E0", -- 0x0408
    x"07",x"07",x"07",x"07",x"07",x"FF",x"FF",x"FF", -- 0x0410
    x"E0",x"E0",x"E0",x"E0",x"E0",x"FF",x"FF",x"FF", -- 0x0418
    x"18",x"18",x"18",x"F8",x"F8",x"18",x"18",x"18", -- 0x0420
    x"18",x"18",x"18",x"1F",x"1F",x"18",x"18",x"18", -- 0x0428
    x"18",x"18",x"18",x"FF",x"FF",x"00",x"00",x"00", -- 0x0430
    x"00",x"00",x"00",x"FF",x"FF",x"18",x"18",x"18", -- 0x0438
    x"00",x"00",x"00",x"FF",x"FF",x"00",x"00",x"00", -- 0x0440
    x"80",x"80",x"80",x"80",x"80",x"80",x"80",x"80", -- 0x0448
    x"01",x"01",x"01",x"01",x"01",x"01",x"01",x"01", -- 0x0450
    x"FF",x"00",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x0458
    x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"FF", -- 0x0460
    x"FF",x"81",x"81",x"81",x"81",x"81",x"81",x"FF", -- 0x0468
    x"18",x"18",x"18",x"18",x"18",x"18",x"18",x"18", -- 0x0470
    x"00",x"00",x"00",x"18",x"18",x"00",x"00",x"00", -- 0x0478
    x"3C",x"3C",x"3C",x"3C",x"3C",x"3C",x"3C",x"3C", -- 0x0480
    x"00",x"00",x"FF",x"FF",x"FF",x"FF",x"00",x"00", -- 0x0488
    x"F8",x"F8",x"C0",x"C0",x"C0",x"C0",x"C0",x"C0", -- 0x0490
    x"1F",x"1F",x"1F",x"03",x"03",x"03",x"03",x"03", -- 0x0498
    x"60",x"60",x"18",x"1E",x"1E",x"18",x"60",x"60", -- 0x04A0
    x"06",x"06",x"18",x"78",x"78",x"18",x"06",x"06", -- 0x04A8
    x"C0",x"C0",x"F0",x"F0",x"F0",x"F0",x"C0",x"C0", -- 0x04B0
    x"03",x"03",x"0F",x"0F",x"0F",x"0F",x"03",x"03", -- 0x04B8
    x"00",x"00",x"3C",x"3C",x"3C",x"3C",x"00",x"00", -- 0x04C0
    x"0C",x"0C",x"3C",x"3C",x"3C",x"3C",x"30",x"30", -- 0x04C8
    x"00",x"00",x"FC",x"FC",x"3F",x"3F",x"00",x"00", -- 0x04D0
    x"C0",x"C0",x"C0",x"C0",x"C0",x"C0",x"FC",x"FC", -- 0x04D8
    x"03",x"03",x"03",x"03",x"03",x"3F",x"3F",x"3F", -- 0x04E0
    x"00",x"18",x"18",x"3C",x"3C",x"C3",x"C3",x"00", -- 0x04E8
    x"00",x"C3",x"C3",x"3C",x"3C",x"18",x"18",x"00", -- 0x04F0
    x"00",x"00",x"00",x"00",x"3C",x"3C",x"FF",x"FF", -- 0x04F8
    x"FF",x"FF",x"3C",x"3C",x"00",x"00",x"00",x"00", -- 0x0500
    x"00",x"7E",x"7E",x"7E",x"7E",x"7E",x"7E",x"00", -- 0x0508
    x"FF",x"FF",x"C0",x"C0",x"C0",x"C0",x"FF",x"FF", -- 0x0510
    x"3C",x"3C",x"3C",x"3C",x"3C",x"3C",x"3C",x"3C", -- 0x0518
    x"FF",x"FF",x"C3",x"C3",x"C3",x"C3",x"C3",x"C3", -- 0x0520
    x"C3",x"C3",x"C3",x"C3",x"C3",x"C3",x"FF",x"FF", -- 0x0528
    x"3C",x"3C",x"FF",x"FF",x"3C",x"3C",x"3C",x"3C", -- 0x0530
    x"24",x"24",x"24",x"E7",x"E7",x"24",x"24",x"24", -- 0x0538
    x"C3",x"C3",x"C3",x"C3",x"C3",x"C3",x"C3",x"C3", -- 0x0540
    x"FF",x"FF",x"FF",x"FF",x"FF",x"FF",x"18",x"18", -- 0x0548
    x"18",x"18",x"FF",x"FF",x"FF",x"FF",x"FF",x"FF", -- 0x0550
    x"C0",x"C0",x"C0",x"C0",x"C0",x"C0",x"C0",x"C0", -- 0x0558
    x"FF",x"FF",x"03",x"03",x"03",x"03",x"FF",x"FF", -- 0x0560
    x"FF",x"FF",x"F0",x"F0",x"F0",x"F0",x"0F",x"0F", -- 0x0568
    x"0F",x"0F",x"F0",x"F0",x"F0",x"F0",x"FF",x"FF", -- 0x0570
    x"FC",x"FC",x"FC",x"FC",x"C3",x"C3",x"C3",x"C3", -- 0x0578
    x"C3",x"C3",x"C3",x"C3",x"FC",x"FC",x"FC",x"FC", -- 0x0580
    x"FC",x"FC",x"FC",x"FC",x"FC",x"FC",x"FC",x"FC", -- 0x0588
    x"C3",x"C3",x"C3",x"C3",x"3F",x"3F",x"3F",x"3F", -- 0x0590
    x"0F",x"0F",x"0F",x"0F",x"33",x"33",x"33",x"33", -- 0x0598
    x"00",x"00",x"00",x"00",x"00",x"00",x"FF",x"FF", -- 0x05A0
    x"FF",x"FF",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x05A8
    x"03",x"03",x"03",x"03",x"03",x"03",x"03",x"03", -- 0x05B0
    x"FF",x"81",x"BD",x"A5",x"A5",x"BD",x"81",x"FF", -- 0x05B8
    x"E7",x"66",x"66",x"66",x"FF",x"FF",x"FF",x"FF", -- 0x05C0
    x"00",x"C3",x"C3",x"3C",x"3C",x"C3",x"C3",x"00", -- 0x05C8
    x"18",x"18",x"18",x"18",x"18",x"18",x"18",x"18", -- 0x05D0
    x"00",x"00",x"00",x"00",x"00",x"00",x"FC",x"FC", -- 0x05D8
    x"00",x"00",x"00",x"00",x"00",x"00",x"3F",x"3F", -- 0x05E0
    x"0F",x"0F",x"0F",x"0F",x"00",x"00",x"00",x"00", -- 0x05E8
    x"00",x"00",x"00",x"00",x"F0",x"F0",x"F0",x"F0", -- 0x05F0
    x"F0",x"F0",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x05F8
    x"18",x"66",x"FF",x"18",x"24",x"42",x"81",x"81", -- 0x0600
    x"00",x"FF",x"00",x"FF",x"00",x"FF",x"00",x"FF", -- 0x0608
    x"18",x"18",x"18",x"FF",x"FF",x"18",x"18",x"18", -- 0x0610
    x"CC",x"CC",x"33",x"33",x"CC",x"CC",x"33",x"33", -- 0x0618
    x"AA",x"55",x"AA",x"55",x"AA",x"55",x"AA",x"55", -- 0x0620
    x"C3",x"66",x"3C",x"18",x"3C",x"66",x"C3",x"81", -- 0x0628
    x"6C",x"FE",x"FE",x"FE",x"7C",x"7C",x"38",x"10", -- 0x0630
    x"81",x"C3",x"DB",x"E7",x"DB",x"C3",x"81",x"00", -- 0x0638
    x"10",x"38",x"7C",x"FE",x"FE",x"6C",x"10",x"7C", -- 0x0640
    x"10",x"38",x"38",x"54",x"FE",x"54",x"10",x"7C", -- 0x0648
    x"FF",x"FF",x"FF",x"FF",x"FF",x"FF",x"FF",x"FF", -- 0x0650
    x"33",x"33",x"33",x"33",x"33",x"33",x"33",x"33", -- 0x0658
    x"92",x"92",x"92",x"92",x"92",x"92",x"92",x"92", -- 0x0660
    x"01",x"03",x"07",x"0F",x"1F",x"3F",x"7F",x"FF", -- 0x0668
    x"FF",x"FF",x"00",x"00",x"FF",x"FF",x"00",x"00", -- 0x0670
    x"C0",x"C0",x"C0",x"C0",x"C0",x"C0",x"C0",x"C0", -- 0x0678
    x"00",x"00",x"FF",x"FF",x"00",x"00",x"FF",x"FF", -- 0x0680
    x"FF",x"FF",x"FF",x"FF",x"00",x"00",x"00",x"00", -- 0x0688
    x"00",x"00",x"00",x"00",x"FF",x"FF",x"FF",x"FF", -- 0x0690
    x"FF",x"FF",x"00",x"00",x"00",x"00",x"00",x"00", -- 0x0698
    x"0F",x"0F",x"0F",x"0F",x"0F",x"0F",x"0F",x"0F", -- 0x06A0
    x"F0",x"F0",x"F0",x"F0",x"F0",x"F0",x"F0",x"F0", -- 0x06A8
    x"F0",x"F0",x"F0",x"F0",x"0F",x"0F",x"0F",x"0F", -- 0x06B0
    x"FF",x"00",x"FF",x"00",x"FF",x"00",x"FF",x"00", -- 0x06B8
    x"B6",x"B6",x"B6",x"B6",x"B6",x"B6",x"B6",x"B6", -- 0x06C0
    x"00",x"00",x"00",x"FF",x"FF",x"00",x"00",x"00", -- 0x06C8
    x"00",x"00",x"00",x"00",x"00",x"00",x"FF",x"FF", -- 0x06D0
    x"03",x"03",x"03",x"03",x"03",x"03",x"03",x"03", -- 0x06D8
    x"04",x"08",x"11",x"22",x"44",x"88",x"10",x"20", -- 0x06E0
    x"CC",x"CC",x"CC",x"CC",x"CC",x"CC",x"CC",x"CC", -- 0x06E8
    x"20",x"10",x"88",x"44",x"22",x"11",x"08",x"04", -- 0x06F0
    x"01",x"03",x"06",x"0C",x"18",x"30",x"60",x"C0", -- 0x06F8
    x"80",x"C0",x"E0",x"F0",x"F8",x"FC",x"FE",x"FF", -- 0x0700
    x"18",x"18",x"18",x"18",x"18",x"18",x"18",x"18", -- 0x0708
    x"FF",x"FE",x"FC",x"F8",x"F0",x"E0",x"C0",x"80", -- 0x0710
    x"C0",x"60",x"30",x"18",x"0C",x"06",x"03",x"01", -- 0x0718
    x"FF",x"7F",x"3F",x"1F",x"0F",x"07",x"03",x"01", -- 0x0720
    x"FF",x"FF",x"7E",x"3C",x"00",x"00",x"00",x"00", -- 0x0728
    x"3C",x"7E",x"FF",x"FF",x"FF",x"FF",x"7E",x"3C", -- 0x0730
    x"99",x"99",x"99",x"99",x"99",x"5A",x"3C",x"18", -- 0x0738
    x"18",x"3C",x"5A",x"99",x"18",x"18",x"18",x"18", -- 0x0740
    x"FF",x"81",x"81",x"99",x"99",x"81",x"81",x"FF", -- 0x0748
    x"C0",x"C0",x"00",x"18",x"18",x"00",x"03",x"03", -- 0x0750
    x"3C",x"7E",x"C3",x"C3",x"C3",x"C3",x"7E",x"3C", -- 0x0758
    x"00",x"00",x"00",x"00",x"0F",x"08",x"08",x"08", -- 0x0760
    x"00",x"00",x"00",x"00",x"F0",x"10",x"10",x"10", -- 0x0768
    x"03",x"07",x"0F",x"0F",x"0F",x"0F",x"07",x"03", -- 0x0770
    x"08",x"08",x"08",x"0F",x"00",x"00",x"00",x"00", -- 0x0778
    x"10",x"10",x"10",x"F0",x"00",x"00",x"00",x"00", -- 0x0780
    x"E7",x"24",x"24",x"3C",x"66",x"C3",x"81",x"81", -- 0x0788
    x"10",x"38",x"7C",x"FE",x"FE",x"7C",x"38",x"10", -- 0x0790
    x"24",x"42",x"A5",x"18",x"18",x"A5",x"42",x"24", -- 0x0798
    x"00",x"00",x"00",x"00",x"3C",x"7E",x"FF",x"FF", -- 0x07A0
    x"C0",x"E0",x"F0",x"F0",x"F0",x"F0",x"E0",x"C0", -- 0x07A8
    x"00",x"00",x"00",x"18",x"18",x"00",x"00",x"00", -- 0x07B0
    x"00",x"66",x"66",x"00",x"00",x"66",x"66",x"00", -- 0x07B8
    x"FF",x"C3",x"81",x"81",x"81",x"81",x"C3",x"FF", -- 0x07C0
    x"C3",x"C3",x"00",x"18",x"18",x"00",x"C3",x"C3", -- 0x07C8
    x"66",x"FF",x"FF",x"66",x"66",x"FF",x"FF",x"66", -- 0x07D0
    x"18",x"18",x"18",x"18",x"99",x"5A",x"3C",x"18", -- 0x07D8
    x"00",x"00",x"18",x"18",x"00",x"18",x"18",x"00", -- 0x07E0
    x"10",x"20",x"40",x"FF",x"FF",x"40",x"20",x"10", -- 0x07E8
    x"66",x"66",x"00",x"66",x"66",x"00",x"66",x"66", -- 0x07F0
    x"08",x"04",x"02",x"FF",x"FF",x"02",x"04",x"08"  -- 0x07F8
  );

begin

  p_rom : process
  begin
    wait until rising_edge(CLK);
     DATA <= ROM(to_integer(unsigned(ADDR)));
  end process;
end RTL;
