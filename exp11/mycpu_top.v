`timescale 1ns / 1ps

module mycpu_top(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_en,
    output wire [ 3:0] inst_sram_we,
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    // data sram interface
    output wire        data_sram_en,
    output wire [ 3:0] data_sram_we,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire [31:0] data_sram_rdata,
    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata
);
reg         reset;
always @(posedge clk) reset <= ~resetn;

// reg         valid;
// always @(posedge clk) begin
//     if (reset) begin
//         valid <= 1'b0;
//     end
//     else begin
//         valid <= 1'b1;
//     end
// end

wire [31:0] seq_pc;
wire [31:0] nextpc;
wire        br_taken;
wire [31:0] br_target;
wire [31:0] inst;
reg  [31:0] pc;

wire [18:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        dst_is_r1;
wire        gr_we;
wire        mem_we;
wire        src_reg_is_rd;
wire [4: 0] dest;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] imm;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;
wire        inst_sub_w;
wire        inst_slt;
wire        inst_sltu;
wire        inst_slti;
wire        inst_sltiu;
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_andi;
wire        inst_ori;
wire        inst_xori;
wire        inst_sll_w;
wire        inst_srl_w;
wire        inst_sra_w;
wire        inst_pcaddu12i;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_b;
wire        inst_ld_h;
wire        inst_ld_bu;
wire        inst_ld_hu;
wire        inst_ld_w;
wire        inst_st_b;
wire        inst_st_h;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_blt;
wire        inst_bge;
wire        inst_bltu;
wire        inst_bgeu;
wire        inst_lu12i_w;
wire        inst_mul_w;
wire        inst_mulh_w;
wire        inst_mulh_wu;
wire        inst_div_w;
wire        inst_mod_w;
wire        inst_div_wu;
wire        inst_mod_wu;

// 该处的信号不仅可以来表示div，mod运算 其实是表示在一个时钟周期内无法获得正确的结果的运算
wire        inst_div_mod;
assign      inst_div_mod = inst_div_w | inst_div_wu | inst_mod_w | inst_mod_wu;
reg         inst_div_mod_EXE;
wire valid_div_mod;


wire        need_ui5;
wire        need_ui12;
wire        need_si12;
wire        need_si16;
wire        need_si20;
wire        need_si26;
wire        src2_is_4;

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;

wire [31:0] alu_src1   ;
wire [31:0] alu_src2   ;
wire [31:0] alu_result ;

wire [31:0] mem_result;

wire [31:0] final_result;

assign seq_pc       = pc + 3'h4;
assign nextpc       = br_taken ? br_target : seq_pc;


wire      validin;
reg       valid_IF;

reg [31:0] pc_ID;
reg        valid_ID;
reg [31:0] inst_ID;

reg [31:0] pc_EXE;
reg        valid_EXE;
reg        gr_we_EXE;
reg [4:0]  dest_EXE;

reg        inst_st_b_EXE;
reg        inst_st_h_EXE;
reg        inst_st_w_EXE;
reg        inst_ld_b_EXE;
reg        inst_ld_bu_EXE;
reg        inst_ld_hu_EXE;
reg        inst_ld_h_EXE;
reg        inst_ld_w_EXE;
reg [31:0] alu_src1_EXE;
reg [31:0] alu_src2_EXE;   
reg [18:0] alu_op_EXE;

reg        data_sram_en_EXE;
reg [31:0] rkd_value_EXE;
reg        res_from_mem_EXE;
reg        mem_we_EXE;

reg        inst_ld_b_MEM;
reg        inst_ld_bu_MEM;
reg        inst_ld_hu_MEM;
reg        inst_ld_h_MEM;
reg        inst_ld_w_MEM;
reg [31:0] pc_MEM;
reg        valid_MEM;
reg        gr_we_MEM;
reg [4:0]  dest_MEM;

reg [31:0] alu_result_MEM;
reg        res_from_mem_MEM;

reg [31:0] pc_WB;
reg        valid_WB;
reg        gr_we_WB;
reg [4:0]  dest_WB;
reg [31:0] final_result_WB;


wire        RAW;
wire        RAW_EXE;
wire        RAW_MEM;
wire        RAW_WB;





wire IFreg_ready_go = 1'b1;
// 定义�?个wire类型的变量IDreg_ready_go，用于表示ID寄存器的就绪状�??
// 如果valid_ID�?0，则IDreg_ready_go�?1
// 如果inst_ld_w_EXE�?1且RAW_EXE�?1，则IDreg_ready_go�?0
// 否则，IDreg_ready_go�?1
wire IDreg_ready_go = (!valid_ID) ? 1'b1: ((inst_ld_w_EXE && RAW_EXE) ? 1'b0 : (valid_EXE ? EXEreg_ready_go:1'b1));
wire EXEreg_ready_go = inst_div_mod_EXE ? valid_div_mod : 1'b1;
wire MEMreg_ready_go = 1'b1;
wire WBreg_ready_go = 1'b1;
wire IFreg_allowin;
wire IDreg_allowin;
wire EXEreg_allowin;
wire MEMreg_allowin;
wire WBreg_allowin;
wire allowout = 1'b1;
assign IFreg_allowin = !valid_IF || (IFreg_ready_go && IDreg_allowin);
assign IDreg_allowin = !valid_ID || (IDreg_ready_go && EXEreg_allowin);
assign EXEreg_allowin = !valid_EXE || (EXEreg_ready_go && MEMreg_allowin);
assign MEMreg_allowin = !valid_MEM || (MEMreg_ready_go && WBreg_allowin);
assign WBreg_allowin = !valid_WB || (WBreg_ready_go && allowout);
wire to_IF_valid;
wire IF_to_ID_valid;
wire ID_to_EXE_valid;
wire EXE_to_MEM_valid;
wire MEM_to_WB_valid;
assign validin = ~reset;
assign to_IF_valid = validin;
assign IF_to_ID_valid = valid_IF && IFreg_ready_go&& !br_taken;
assign ID_to_EXE_valid = valid_ID && IDreg_ready_go;
assign EXE_to_MEM_valid = valid_EXE && EXEreg_ready_go;
assign MEM_to_WB_valid = valid_MEM && MEMreg_ready_go;




// 区分指令读取的寄存器
wire is_rj_rk_inst = inst_add_w | inst_sub_w | inst_slt | inst_sltu | inst_and | inst_or | inst_nor | inst_xor | inst_sll_w | inst_srl_w | inst_sra_w |inst_mul_w | inst_mulh_w |inst_mulh_wu | inst_div_w |inst_div_wu |inst_mod_w |inst_mod_wu;//�?? rj rk
wire is_rj_imm_inst = inst_addi_w | inst_slli_w | inst_srli_w | inst_srai_w | inst_jirl | inst_ld_w | inst_ld_b| inst_ld_bu| inst_ld_h| inst_ld_hu |inst_sltui | inst_slti | inst_andi | inst_ori | inst_xori ;//�?? rj
wire is_rj_rd_inst = inst_beq | inst_bne | inst_st_w | inst_st_b| inst_st_h | inst_blt | inst_bge | inst_bltu | inst_bgeu ;//�?? rj rd
wire is_rd_inst = 1'b0; //

// 有关读后写的冲突信号
assign RAW_EXE = gr_we_EXE & valid_EXE & ((is_rj_rk_inst & ((rj == dest_EXE) | (rk == dest_EXE))) | (is_rj_imm_inst & (rj == dest_EXE)) | (is_rj_rd_inst & ((rj == dest_EXE) | (rd == dest_EXE))) | (is_rd_inst & (rd == dest_EXE)));
assign RAW_MEM = gr_we_MEM & valid_MEM & ((is_rj_rk_inst & ((rj == dest_MEM) | (rk == dest_MEM))) | (is_rj_imm_inst & (rj == dest_MEM)) | (is_rj_rd_inst & ((rj == dest_MEM) | (rd == dest_MEM))) | (is_rd_inst & (rd == dest_MEM)));
assign RAW_WB = gr_we_WB & valid_WB & ((is_rj_rk_inst & ((rj == dest_WB) | (rk == dest_WB))) | (is_rj_imm_inst & (rj == dest_WB)) | (is_rj_rd_inst & ((rj == dest_WB) | (rd == dest_WB))) | (is_rd_inst & (rd == dest_WB)));
assign RAW = RAW_EXE | RAW_MEM | RAW_WB;

// 有关数据前递?�的信号

// EXE阶段�?01 MEM阶段�?10 WB阶段�?11 并且为优先编码流水线前级操作
wire [1:0] sel_fwd_1;
assign sel_fwd_1 = (gr_we_EXE & valid_EXE & ((is_rj_rk_inst & (rj == dest_EXE)) | (is_rj_imm_inst & (rj == dest_EXE)) | (is_rj_rd_inst & (rj == dest_EXE)))) ? 2'b01 :
                   ((gr_we_MEM & valid_MEM & ((is_rj_rk_inst & (rj == dest_MEM)) | (is_rj_imm_inst & (rj == dest_MEM)) | (is_rj_rd_inst & (rj == dest_MEM)))) ? 2'b10 :
                   ((gr_we_WB & valid_WB & ((is_rj_rk_inst & (rj == dest_WB)) | (is_rj_imm_inst & (rj == dest_WB)) | (is_rj_rd_inst & (rj == dest_WB)))) ? 2'b11 : 2'b00));
wire [1:0] sel_fwd_2;
assign sel_fwd_2 = (gr_we_EXE & valid_EXE & ((is_rj_rk_inst & (rk == dest_EXE)) | (is_rj_rd_inst & (rd == dest_EXE)) | (is_rd_inst & (rd == dest_EXE)))) ? 2'b01 :
                   ((gr_we_MEM & valid_MEM & ((is_rj_rk_inst & (rk == dest_MEM)) | (is_rd_inst & (rd == dest_MEM)) | (is_rj_rd_inst & (rd == dest_MEM)))) ? 2'b10 :
                   ((gr_we_WB & valid_WB & ((is_rj_rk_inst & (rk == dest_WB)) | (is_rd_inst & (rd == dest_WB)) | (is_rj_rd_inst & (rd == dest_WB)))) ? 2'b11 : 2'b00));

wire [31:0] fwddata_EXE = alu_result;
wire [31:0] fwddata_MEM = final_result;
wire [31:0] fwddata_WB = final_result_WB;


// IF
always @(posedge clk) begin
    if (reset) begin
        pc <= 32'h1bfffffc;
        valid_IF <= 1'b0;
    end
    
    else if(IFreg_allowin & to_IF_valid)begin
        valid_IF <= to_IF_valid;
    end

    else if(br_taken)begin
        valid_IF <= 1'b0;
    end

    if(to_IF_valid && IFreg_allowin&& IDreg_ready_go) begin
        pc <= nextpc;
        
    end
    

end


//ID
always @(posedge clk) begin
    if(reset) begin
        valid_ID <= 1'b0;
    end
    else if(br_taken && IDreg_ready_go) begin
        valid_ID <= 1'b0;
    end
    else if(IDreg_allowin)begin
        valid_ID <= IF_to_ID_valid;
    end

    if(IF_to_ID_valid && IDreg_allowin )begin
        pc_ID <= pc;
        inst_ID <= inst;
    end

end

//EXE
always @(posedge clk) begin
    if(reset) begin
        valid_EXE <= 1'b0;
    end
    else if (EXEreg_allowin)begin
        valid_EXE <= ID_to_EXE_valid;
    end

    if(ID_to_EXE_valid && EXEreg_allowin)begin
        pc_EXE <= pc_ID;
        gr_we_EXE <= gr_we;
        dest_EXE <= dest;
        
        inst_st_b_EXE <= inst_st_b;
        inst_st_h_EXE <= inst_st_h;
        inst_st_w_EXE <= inst_st_w;
        inst_ld_w_EXE <= inst_ld_w;
        inst_ld_b_EXE <= inst_ld_b;
        inst_ld_bu_EXE <= inst_ld_bu;
        inst_ld_hu_EXE <= inst_ld_hu;
        inst_ld_h_EXE <= inst_ld_h;
        res_from_mem_EXE <= res_from_mem;
        alu_src1_EXE <= alu_src1;
        alu_src2_EXE <= alu_src2;
        alu_op_EXE <= alu_op;
        
        data_sram_en_EXE <= (mem_we || inst_ld_w || inst_ld_b || inst_ld_bu || inst_ld_hu || inst_ld_h || inst_st_b || inst_st_h || inst_st_w);
        rkd_value_EXE <= rkd_value;
        mem_we_EXE <= mem_we;
        inst_div_mod_EXE <= inst_div_mod;
    end
end

//MEM
always @(posedge clk) begin
    if(reset) begin
        valid_MEM <= 1'b0;
    end
    else if (MEMreg_allowin) begin
        valid_MEM <= EXE_to_MEM_valid;
    end

    if(EXE_to_MEM_valid && MEMreg_allowin)begin
        pc_MEM <= pc_EXE;
        gr_we_MEM <= gr_we_EXE;
        dest_MEM <= dest_EXE;
        
        res_from_mem_MEM <= res_from_mem_EXE;
        alu_result_MEM <= alu_result;

        inst_ld_b_MEM <= inst_ld_b_EXE;
        inst_ld_bu_MEM <= inst_ld_bu_EXE;
        inst_ld_h_MEM <= inst_ld_h_EXE;
        inst_ld_hu_MEM <= inst_ld_hu_EXE;
        inst_ld_w_MEM <= inst_ld_w_EXE;
        
    end
end



//WB
always @(posedge clk) begin
    if(reset) begin
        valid_WB <= 1'b0;
    end
    else if (WBreg_allowin) begin
        valid_WB <= MEM_to_WB_valid;
    end

    if(MEM_to_WB_valid && WBreg_allowin) begin
        pc_WB <= pc_MEM;
        gr_we_WB <= gr_we_MEM;
        dest_WB <= dest_MEM;
        final_result_WB <= final_result;
    end
end



assign inst_sram_en    = br_taken ? 1'b1 : IDreg_ready_go;
assign inst_sram_we    = 1'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;
assign inst           = inst_sram_rdata;

assign op_31_26  = inst_ID[31:26];
assign op_25_22  = inst_ID[25:22];
assign op_21_20  = inst_ID[21:20];
assign op_19_15  = inst_ID[19:15];

assign rd   = inst_ID[ 4: 0];
assign rj   = inst_ID[ 9: 5];
assign rk   = inst_ID[14:10];

assign i12  = inst_ID[21:10];
assign i20  = inst_ID[24: 5];
assign i16  = inst_ID[25:10];
assign i26  = {inst_ID[ 9: 0], inst_ID[25:10]};

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));

assign inst_add_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];
assign inst_slti   = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui  = op_31_26_d[6'h00] & op_25_22_d[4'h9];

assign inst_nor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];
assign inst_andi   = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori    = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori   = op_31_26_d[6'h00] & op_25_22_d[4'hf];

assign inst_sll_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_srl_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_sra_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];

assign inst_slli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];

assign inst_addi_w = op_31_26_d[6'h00] & op_25_22_d[4'ha];

assign inst_ld_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
assign inst_ld_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_bu  = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
assign inst_ld_hu  = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
assign inst_st_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h5];
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];

assign inst_jirl   = op_31_26_d[6'h13];
assign inst_b      = op_31_26_d[6'h14];
assign inst_bl     = op_31_26_d[6'h15];
assign inst_beq    = op_31_26_d[6'h16];
assign inst_bne    = op_31_26_d[6'h17];
assign inst_blt    = op_31_26_d[6'h18];
assign inst_bge    = op_31_26_d[6'h19];
assign inst_bltu   = op_31_26_d[6'h1a];
assign inst_bgeu   = op_31_26_d[6'h1b];
assign inst_lu12i_w= op_31_26_d[6'h05] & ~inst_ID[25];
assign inst_pcaddu12i= op_31_26_d[6'h07] & ~inst_ID[25];

assign inst_mul_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_div_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_mod_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_div_wu  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mod_wu  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu
                    | inst_st_b | inst_st_h
                    | inst_jirl | inst_bl | inst_pcaddu12i;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w | inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;
assign alu_op[12] = inst_mul_w;
assign alu_op[13] = inst_mulh_w;
assign alu_op[14] = inst_mulh_wu;
assign alu_op[15] = inst_div_w;
assign alu_op[16] = inst_div_wu;
assign alu_op[17] = inst_mod_w;
assign alu_op[18] = inst_mod_wu;


// 指令所需的立即数格式
assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_ui12  =  inst_andi | inst_ori | inst_xori;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w | inst_ld_b 
                    | inst_ld_h | inst_ld_bu | inst_ld_hu | inst_st_b 
                    | inst_st_h | inst_slti |inst_sltui;
assign need_si16  =  inst_jirl | inst_beq | inst_bne | inst_blt | inst_bge | inst_bltu | inst_bgeu;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_ui12 ? {20'b0,i12 }               :
/*need_ui5 || need_si12*/{{20{i12[11]}}, i12[11:0]} ;

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};



assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

assign src2_is_imm   = inst_slli_w |
                       inst_srli_w |
                       inst_srai_w |
                       inst_addi_w |
                       inst_ld_w   |
                       inst_ld_b   | 
                       inst_ld_h   | 
                       inst_ld_bu  | 
                       inst_ld_hu  |
                       inst_st_w   |
                       inst_st_b   |
                       inst_st_h   |
                       inst_lu12i_w|
                       inst_jirl   |
                       inst_bl     |
                       inst_slti   |
                       inst_sltui  |
                       inst_andi   |
                       inst_ori    |
                       inst_xori   |
                       inst_pcaddu12i;
                       
                       

assign res_from_mem  = inst_ld_w | inst_ld_b | inst_ld_bu | inst_ld_hu | inst_ld_h;   // 指示是否从内存中读取数据
assign dst_is_r1     = inst_bl;     // 指示写入的寄存器是否为r1
// assign gr_we         = inst_add_w | inst_addi_w | inst_sub_w | inst_bl | inst_jirl | inst_slt | inst_sltu | inst_slli_w | inst_srli_w | inst_srai_w | inst_lu12i_w | inst_or | inst_and | inst_xor | inst_nor | inst_ld_w;
assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b & ~inst_blt &
                        ~inst_st_b & ~inst_st_h & ~inst_bltu & ~inst_bge & ~inst_bgeu;
// 指示是否写入寄存器文�??
assign mem_we        = inst_st_w | inst_st_b | inst_st_h;  // 指示是否向内存中写入数据
assign dest          = dst_is_r1 ? 5'd1 : rd;   // 指示写入的寄存器编号

assign rf_raddr1 = rj;  // 指示读取的寄存器编号
assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w | inst_st_b | inst_st_h | inst_blt | inst_bltu | inst_bge | inst_bgeu;
assign rf_raddr2 = src_reg_is_rd ? rd :rk; // 指示读取的寄存器编号
regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

assign rj_value  = {32{(!sel_fwd_1[1] & sel_fwd_1[0])}} & fwddata_EXE |
                   {32{(sel_fwd_1[1] & !sel_fwd_1[0])}} & fwddata_MEM |
                   {32{(sel_fwd_1[1] & sel_fwd_1[0])}} & fwddata_WB |
                   {32{(!sel_fwd_1[1] & !sel_fwd_1[0])}} & rf_rdata1;
assign rkd_value = {32{(!sel_fwd_2[1] & sel_fwd_2[0])}} & fwddata_EXE |
                   {32{(sel_fwd_2[1] & !sel_fwd_2[0])}} & fwddata_MEM |
                   {32{(sel_fwd_2[1] & sel_fwd_2[0])}} & fwddata_WB |
                   {32{(!sel_fwd_2[1] & !sel_fwd_2[0])}} & rf_rdata2;


wire rj_eq_rd;
assign rj_eq_rd = (rj_value == rkd_value);

wire [32:0] sub_result;
wire        rj_lt_rd;
wire        rj_ltu_rd;

assign sub_result = {1'b0, rj_value} + {1'b0, ~rkd_value} + 1'b1;


// SLT result
assign rj_lt_rd = (rj_value[31] & ~rkd_value[31])
                        | ((rj_value[31] ~^ rkd_value[31]) & sub_result[31]);

// SLTU result
assign rj_ltu_rd = ~sub_result[32];



assign br_taken = (   inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
                   || inst_blt  &&  rj_lt_rd
                   || inst_bge  && !rj_lt_rd
                   || inst_bltu &&  rj_ltu_rd
                   || inst_bgeu && !rj_ltu_rd
                   || inst_jirl
                   || inst_bl
                   || inst_b
                  ) && valid_ID && IDreg_ready_go; // 指示是否跳转
assign br_target = (inst_beq || inst_bne || inst_bl || inst_b || inst_blt || inst_bge || inst_bltu || inst_bgeu) ? (pc_ID + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);

assign alu_src1 = src1_is_pc  ? pc_ID[31:0] : rj_value;
assign alu_src2 = src2_is_imm ? imm : rkd_value;

alu u_alu(
    .clk        (clk        ),
    .resetn      (resetn     ),
    .EXEreg_ready_go(EXEreg_ready_go),
    .MEMreg_allowin(MEMreg_allowin),
    .valid_EXE (valid_EXE  ),
    .alu_op     (alu_op_EXE    ),
    .alu_src1   (alu_src1_EXE  ),
    .alu_src2   (alu_src2_EXE  ),
    .alu_result (alu_result),
    .valid_div_mod(valid_div_mod)
    );

wire [31:0] st_data;  // 写入存储器的数据
assign st_data = inst_st_b_EXE ? {4{rkd_value_EXE[7:0]}} :
                 inst_st_h_EXE ? {2{rkd_value_EXE[15:0]}}:
                                    rkd_value_EXE[31:0] ;



// store  在exe阶段确定写入data_sram的地址和数据，所以参与的信号都是*EXE
wire [1:0] st_addr;
wire [3:0] st_op;
wire [3:0] st_b_op;
wire [3:0] st_h_op;
wire [3:0] st_w_op;
wire [31:0] st_data;

// 根据地址的低两位确定访问哪块data_sram，即确定data_sram_we的值，四位，每位确定一个数据sram的写使能，4位对应4个字节，即32位
assign st_addr = alu_result[1:0];
assign st_b_op = {4{(!st_addr[0] & !st_addr[1])}} & 4'b0001 |
                 {4{( st_addr[0] & !st_addr[1])}} & 4'b0010 |
                 {4{(!st_addr[0] &  st_addr[1])}} & 4'b0100 |
                 {4{( st_addr[0] &  st_addr[1])}} & 4'b1000 ;
assign st_h_op = (!st_addr[0] & !st_addr[1]) ? 4'b0011 : 4'b1100;
assign st_w_op = 4'b1111;

assign st_op   = {4{inst_st_b_EXE}} & st_b_op |
                 {4{inst_st_h_EXE}} & st_h_op |
                 {4{inst_st_w_EXE}} & st_w_op ;

assign st_data = inst_st_b_EXE ? {4{rkd_value_EXE[7:0]}} :
                 inst_st_h_EXE ? {2{rkd_value_EXE[15:0]}}:
                                    rkd_value_EXE[31:0] ;


assign data_sram_we    = (valid_EXE & mem_we_EXE) ? st_op : 4'h0;
assign data_sram_addr  = alu_result;
assign data_sram_wdata = st_data;

assign data_sram_en    = data_sram_en_EXE;
// assign data_sram_we    = data_sram_we_EXE;
// assign data_sram_addr  = alu_result; //将alu_result作为内存地址
// assign data_sram_wdata = st_data; // 将rkd_value作为写入内存的数�??
// assign mem_result   = data_sram_rdata;

// wire [31:0] load_data;  // 从存储器读取的 32 位数据
// assign load_data = mem_result;
// // wire [31:0] vaddr;      // 虚拟地址
// // assign vaddr = alu_result;
// wire [1:0]  addr;       // 低两位地址，决定访问的是哪个字节或半字

// assign addr = alu_result[1:0];  // 低两位用于字节对齐

// wire [31:0] ld_result;  // 从存储器读取的 32 位数据
// assign ld_result =
//     ({32{inst_ld_w_MEM }} & load_data) |  // 直接加载整个字
//     ({32{inst_ld_h_MEM }} & {{16{load_data[{addr[1], 4'b0} + 15]}}, load_data[{addr[1], 4'b0} +: 16]}) | // 半字，符号扩展
//     ({32{inst_ld_hu_MEM}} & {16'b0, load_data[{addr[1], 4'b0} +: 16]}) | // 半字，无符号扩展
//     ({32{inst_ld_b_MEM }} & {{24{load_data[{addr, 3'b0} + 7]}}, load_data[{addr, 3'b0} +: 8]}) | // 字节，符号扩展
//     ({32{inst_ld_bu_MEM}} & {24'b0, load_data[{addr, 3'b0} +: 8]}); // 字节，无符号扩展



// load  在exe阶段对data_sram的信号线确定，在mem阶段得到data_sram_rdata，所以后续参与的信号都是*MEM
wire [1:0] ld_addr;
wire [7:0] mem_byte;
wire [15:0] mem_half;
wire [31:0] mem_word;
wire [31:0] ld_b_res;
wire [31:0] ld_h_res;
wire [31:0] ld_w_res;

assign ld_addr = alu_result_MEM[1:0];
assign mem_byte = {8{!ld_addr[0] & !ld_addr[1]}} & data_sram_rdata[7:0]   |
                  {8{ ld_addr[0] & !ld_addr[1]}} & data_sram_rdata[15:8]  |
                  {8{!ld_addr[0] &  ld_addr[1]}} & data_sram_rdata[23:16] |
                  {8{ ld_addr[0] &  ld_addr[1]}} & data_sram_rdata[31:24] ;
assign mem_half = ld_addr[1] ? data_sram_rdata[31:16] : data_sram_rdata[15:0];
assign mem_word = data_sram_rdata;
assign ld_b_res[31:8] = {24{inst_ld_b_MEM & mem_byte[7]}};
assign ld_b_res[7:0]  = mem_byte;
assign ld_h_res[31:16]= {16{inst_ld_h_MEM & mem_half[15]}};
assign ld_h_res[15:0] = mem_half;
assign ld_w_res       = mem_word;

assign mem_result = {32{inst_ld_b_MEM || inst_ld_bu_MEM}} & ld_b_res |
                    {32{inst_ld_h_MEM || inst_ld_hu_MEM}} & ld_h_res |
                    {32{inst_ld_w_MEM}}                  & ld_w_res ;

assign final_result = res_from_mem_MEM ? mem_result : alu_result_MEM;



assign rf_we    = (valid_WB) ? gr_we_WB : 1'b0;
assign rf_waddr = dest_WB;
assign rf_wdata = final_result_WB;

// debug info generate
assign debug_wb_pc       = pc_WB;  // 写回阶段的pc
assign debug_wb_rf_we    = {4{rf_we}};
assign debug_wb_rf_wnum  = dest_WB; // 写回阶段的写入寄存器编号
assign debug_wb_rf_wdata = final_result_WB; // 写回阶段的写入数�??





endmodule
