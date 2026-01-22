import tkinter as tk
from tkinter import filedialog
from PIL import Image, ImageTk
import os

class Card:
    def __init__(self, caption, atk_points, image_path):
        self.caption = caption
        self.atk_points = atk_points
        self.image_path = image_path

class DeckBuilderApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Deck Builder")
        self.deck = []

        # Input fields
        tk.Label(root, text="Caption:").grid(row=0, column=0)
        self.caption_entry = tk.Entry(root)
        self.caption_entry.grid(row=0, column=1)

        tk.Label(root, text="ATK Points:").grid(row=1, column=0)
        self.atk_entry = tk.Entry(root)
        self.atk_entry.grid(row=1, column=1)

        self.image_path = None
        tk.Button(root, text="Choose Image", command=self.choose_image).grid(row=2, column=0, columnspan=2)
        tk.Button(root, text="Add Card", command=self.add_card).grid(row=3, column=0, columnspan=2)

        # Save deck button
        tk.Button(root, text="Save Deck", command=self.save_deck).grid(row=4, column=0, columnspan=2)

        # Listbox for deck
        self.deck_listbox = tk.Listbox(root, width=40, height=10)
        self.deck_listbox.grid(row=5, column=0, columnspan=2)
        self.deck_listbox.bind("<<ListboxSelect>>", self.show_card)

        # Separate preview frame
        self.preview_frame = tk.Frame(root, bd=2, relief="groove")
        self.preview_frame.grid(row=6, column=0, columnspan=2, pady=10)

        self.preview_caption = tk.Label(self.preview_frame, text="", font=("Arial", 14))
        self.preview_caption.pack()

        self.preview_atk = tk.Label(self.preview_frame, text="", font=("Arial", 12))
        self.preview_atk.pack()

        self.preview_image_label = tk.Label(self.preview_frame)
        self.preview_image_label.pack()

        self.center_window()

    def center_window(self):
        """Resize and center the window to fit all widgets."""
        self.root.update_idletasks()  # force layout update
        window_width = self.root.winfo_width()
        window_height = self.root.winfo_height() + 70
        screen_width = self.root.winfo_screenwidth()
        screen_height = self.root.winfo_screenheight()
        x = (screen_width // 2) - (window_width // 2)
        y = (screen_height // 2) - (window_height // 2)
        self.root.geometry(f"{window_width}x{window_height}+{x}+{y}")

    def choose_image(self):
        self.image_path = filedialog.askopenfilename(filetypes=[("Image files", "*.png;*.jpg;*.jpeg")])

    def add_card(self):
        caption = self.caption_entry.get()
        atk_points = self.atk_entry.get()
        if not caption or not atk_points or not self.image_path:
            return

        card = Card(caption, int(atk_points), self.image_path)
        self.deck.append(card)

        # Add to listbox
        self.deck_listbox.insert(tk.END, f"{caption} (ATK {atk_points})")

    def show_card(self, event):
        selection = self.deck_listbox.curselection()
        if not selection:
            return

        index = selection[0]
        card = self.deck[index]

        # Update preview
        self.preview_caption.config(text=card.caption)
        self.preview_atk.config(text=f"ATK: {card.atk_points}")

        img = Image.open(card.image_path).resize((100, 150))
        photo = ImageTk.PhotoImage(img)
        self.preview_image_label.config(image=photo)
        self.preview_image_label.image = photo  # keep reference

        self.center_window()

    def save_deck(self):
        if not self.deck:
            return

        file_path = filedialog.asksaveasfilename(defaultextension=".txt",
                                                 filetypes=[("Text files", "*.txt")])
        if not file_path:
            return

        with open(file_path, "w", encoding="utf-8") as f:
            for card in self.deck:
                image_name = os.path.basename(card.image_path)  # only filename
                f.write(f"{image_name} {card.caption} {card.atk_points}\n")

        print("Deck saved!")


if __name__ == "__main__":
    root = tk.Tk()
    app = DeckBuilderApp(root)
    root.mainloop()
