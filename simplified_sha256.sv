module simplified_sha256 #(parameter integer size = 80)( 
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, READ, BLOCK, COMPUTE, WRITE} state;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
//logic [31:0] message[20];
//logic [31:0] wt;
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
//logic [512:0] memory_block;
logic [ 7:0] tstep;

//logic [31:0] TEMPORARY[8]; //ZACK ADDED THIS TO TEST A NEW SOLUTION FOR WRITING SECTION
//logic [15:0] temporary_write_address; //ZACK ADDED THIS TO TEST A NEW SOLUTION FOR WRITING SECTION


// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


assign num_blocks = determine_num_blocks(size); 
assign tstep = (i - 1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);

  if (size%64 <56 )				            // hint: block size = 64
    determine_num_blocks = size/64 + 1;
  else 
    determine_num_blocks = size/64 + 2;   
  $display("size = %d, num_blocks = %d",size,determine_num_blocks);

endfunction


// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    // Student to add remaning code below
    ch = (e&f) ^ ((~e)&g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a,2) ^ rightrotate(a,13) ^ rightrotate(a,22);
    maj = (a&b) ^ (a&c) ^ (b&c);
    t2 = S0 + maj;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;


// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
   
   rightrotate = (x>>r)|(x<<(32-r));

endfunction

// Called from COMPUTE stage for word expansion logic
function logic [31:0] mask_data(input logic [31:0] data,
                                input logic [ 1:0] select);
    case (select)
    0: mask_data = 32'h80000000;
    1: mask_data = data & 32'h FF000000 | 32'h 00800000;
    2: mask_data = data & 32'h FFFF0000 | 32'h 00008000;
    3: mask_data = data & 32'h FFFFFF00 | 32'h 00000080;
    endcase
endfunction

// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
  end 
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin 
       if(start) begin
       //Initialize hash values:
       //(first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19):
          h0 <= 32'h6a09e667;
          h1 <= 32'hbb67ae85;
          h2 <= 32'h3c6ef372;
          h3 <= 32'ha54ff53a;
          h4 <= 32'h510e527f;
          h5 <= 32'h9b05688c;
          h6 <= 32'h1f83d9ab;
          h7 <= 32'h5be0cd19;

		// Initialize number of block iterations variable
       j              <= 0; 

		// Initialize pointer to access memory location
	   offset         <= 0;

		// by default, set memory to read mode 
	   cur_we         <= 0;

		// get staring address of message 
	   cur_addr       <= message_addr;

		// clear write data to memory
	   cur_write_data <= 0;

		// on to block fetch
	   state          <= BLOCK;

       end
    end

    // SHA-256 FSM 
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
    // and write back hash value back to memory
    BLOCK: begin
	// Fetch message in 512-bit block size
	// For each of 512-bit block initiate hash value computation
      if(j < num_blocks) begin
		// at start of processing each Mj, initialize
       	a <= h0; 
		b <= h1;
		c <= h2;
		d <= h3;
		e <= h4;
		f <= h5;
		g <= h6;
		h <= h7;
		
		// reset 64 iteration processing counter 
		i <= 0;

		// increment memory address to fetch next block
		offset <= offset+1;

		// move to SHA256 hash computation stage
		state  <= COMPUTE; 
	  end
		// After all blocks from memory are fetched and hash value is computed,
		//  move to memory write stage to write final 256-bit computed hash value
	  else begin
		offset <= 0;
	    i <= 0;
		state <= WRITE ;
		end
	end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
	// 64 processing rounds steps for 512-bit block 
        if (i <= 64) begin
// for each proc step i perform word expansion for wt
          for (int n = 0; n < 15; n++) w[n] <= w[n+1];
	    // If i < 16, Wi = i'th 32-bit word of block Mi
            if (i < 16) begin
                if ((offset << 2) <= size)
                    w[15] <= mem_read_data; // read ahead
                else if ((offset << 2) > size && ((offset - 1) << 2) <= size)
                    w[15] <= mask_data(mem_read_data, size[1:0]);
                else if ((j == num_blocks - 1) && i == 15) // if last block and last word
                    w[15] <= size << 3; // pad message size in bits
                else
                    w[15] <= 32'h00000000;
            end
	    // Extend the first 16 words into the remaining 48 words w[15..63] of the message schedule array
	    else begin
	    // w[i] := w[i-15] + s0 + w[i-6] + s1
            w[15] <= w[0] + (rightrotate(w[1],7)^ rightrotate(w[1], 18) ^ (w[1] >> 3))
					+ w[9] + (rightrotate(w[14],17)^ rightrotate(w[14], 19) ^ (w[14] >> 10) );// function of w[0], right-rotated/shifted w[1], w[9], right-rotated/shifted w[14] 

            end

            if (i < 15) offset <= offset+1;

            if (i != 0) begin // process tstep = (i - 1)th round
                {a,b,c,d,e,f,g,h} <= sha256_op(a,b,c,d,e,f,g,h, w[15], tstep);
            end

	    // next processing iteration
            i <= i+1;
				
	    // Stay in COMPUTE stage until 64 iterations of processing is completed for the BLOCK
	    state <= COMPUTE;
        end else begin
            // Once all 64 rounds of processing completed 
            // Add the compressed chunk computed from sha256_a function to the current hash value
            // and compute final hash value for current block
            h0 <= h0 + a;
			h1 <= h1 + b;
			h2 <= h2 + c;
			h3 <= h3 + d;
			h4 <= h4 + e;
			h5 <= h5 + f;
			h6 <= h6 + g;
			h7 <= h7 + h;

	    // increment block index
            j <= j+1;  // not sure

            // go to next BLOCK fetch stage
            state <= BLOCK;

        end
    end

    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr

    WRITE: begin
	if(i<8) begin
	// Generate write request to memory starting from output_addr
                cur_we <= 1;
                cur_addr <= output_addr;
                case(i)
                  0: cur_write_data <= h0;
				  1: cur_write_data <= h1;
				  2: cur_write_data <= h2;
				  3: cur_write_data <= h3;
				  4: cur_write_data <= h4;
				  5: cur_write_data <= h5;
				  6: cur_write_data <= h6;
				  7: cur_write_data <= h7;
	endcase
  // update memory address
        offset <= i; 
     
  // increment hash pointer
        i <= i+1; 
		end else 
  //  proceed to next state
	    state <= IDLE;
	end
	endcase
    end 

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
