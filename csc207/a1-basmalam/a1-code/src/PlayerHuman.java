import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

//TODO: Add Javadoc comments for this class and all its methods. (Task 3)
/**
 * 
 * This class represents the human player in the game Othello
 * 
 * It keeps track of the character that represents them and the move they want to make.
 *
 */
public class PlayerHuman {
	
	private static final String INVALID_INPUT_MESSAGE = "Invalid number, please enter 1-8";
	private static final String IO_ERROR_MESSAGE = "I/O Error";
	private static BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

	private Othello othello;
	private char player;

	/**
	 * Constructs the PlayerHuman object and initializes their player char and the game that they are linked to.
	 * 
	 * @param othello the Instance of the game Othello that this player will be linked to 
	 * @param player the character that will represent the player
	 */
	public PlayerHuman(Othello othello, char player) {
		
		this.othello = othello;
		this.player = player;
	}

	/**
	 * Returns a Move object containing the row and column that the player specifies
	 * 
	 * @return  Move object that contains information about row and column 
	 * where the player wants to place their piece
	 */
	public Move getMove() {
		
		int row = getMove("row: ");
		int col = getMove("col: ");
		return new Move(row, col);
	}

	/**
	 * Checks the information gathered from the user and prints a prompt/message accordingly is the input is invalid
	 * 
	 * @param message contains the information that will be displayed to the user
	 * @return returns -1 when the information that is received is valid
	 */
	private int getMove(String message) {
		
		int move, lower = 0, upper = 7;
		while (true) {
			try {
				System.out.print(message);
				String line = PlayerHuman.stdin.readLine();
				move = Integer.parseInt(line);
				if (lower <= move && move <= upper) {
					return move;
				} else {
					System.out.println(INVALID_INPUT_MESSAGE);
				}
			} catch (IOException e) {
				System.out.println(INVALID_INPUT_MESSAGE);
				break;
			} catch (NumberFormatException e) {
				System.out.println(INVALID_INPUT_MESSAGE);
			}
		}
		return -1;
	}
}
