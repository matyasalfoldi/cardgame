#pragma once
#include <cstdint>
#include <vector>

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/fl_message.H>
#include <FL/Fl_Box.H>

#include "Model.h"
#include "View.h"

class AppController;

struct CardCallbackData {
    AppController* controller;
    int index;
};


class AppController {
    AppModel* model;
    const int NUM_CARDS = 3;
    Fl_Window* menu_win = nullptr;
    Fl_Window* main_win = nullptr;
    std::vector<CardWidget*> player_cards;
    Fl_Box* player_points_label = nullptr;
    Fl_Box* npc_points_label = nullptr;
    Fl_Box* turn_label = nullptr;
    CardWidget* player_chosen = nullptr;
    CardWidget* npc_chosen = nullptr;
    CardWidget* player_deck = nullptr;
    CardWidget* npc_deck = nullptr;
    std::vector<CardCallbackData> card_callback_data;

public:
    AppController() {
        init();

        // Build menu view
        menu_win = new Fl_Window(Fl::w(), Fl::h(), "Start");
        Fl_Button* start_btn = new Fl_Button(Fl::w() / 2, Fl::h() / 2, 100, 41, "Start");
        start_btn->callback(start_cb, this);
        menu_win->end();
        menu_win->show();
    }

    void init()
    {
        model = new AppModel();
        for (size_t i = 0; i < NUM_CARDS; i++)
        {
            model->draw_card();
        }
        model->fill_player_and_npc_deck_count();
    }

    static void start_cb(Fl_Widget*, void* userdata) {
        auto* self = static_cast<AppController*>(userdata);
        self->init();
        self->show_game();
    }

    static void back_cb(Fl_Widget*, void* userdata) {
        auto* self = static_cast<AppController*>(userdata);
        if (self->model)
            delete self->model;
        self->show_menu();
    }

    void show_game() {
        menu_win->hide();
        if (main_win) { delete main_win; main_win = nullptr; }

        main_win = new Fl_Window(Fl::w(), Fl::h(), "Game");
        Fl_Button* back_btn = new Fl_Button(main_win->w() - 150, 0, 100, 40, "Back");
        back_btn->callback(back_cb, this);

        npc_deck = new CardWidget(50, 50, 150, 200);
        npc_deck->set_model(model->npc_deck_count_card);

        std::vector<CardWidget*> opponent_cards;

        for (size_t i = 1; i <= NUM_CARDS; ++i)
        {
            opponent_cards.push_back(new CardWidget((Fl::w() / 2) + 250 - (i * 200), 25, 150, 200));
            opponent_cards[i - 1]->set_model(model->card);
        }

        npc_chosen = new CardWidget(Fl::w() / 2 - 150, 200, 150, 200);
        npc_chosen->set_model(model->npc_chosen);

        player_deck = new CardWidget(Fl::w() - 200, Fl::h() - 200, 150, 200);
        player_deck->set_model(model->player_deck_count_card);
        
        player_cards.clear();
        player_cards.resize(3);
        card_callback_data.resize(NUM_CARDS);
        for (int i = 0; i < NUM_CARDS; ++i)
        {
            int index1based = i + 1;
            player_cards[i] = new CardWidget((Fl::w() / 2) + 250 - ((i+1) * 200), Fl::h() - 200, 150, 200);
            player_cards[i]->set_model(model->player_cards[i]);

            card_callback_data[i] = CardCallbackData{ this, index1based };

            player_cards[i]->callback(card_click_cb, &card_callback_data[i]);
        }

        player_chosen = new CardWidget(Fl::w() / 2 - 150, Fl::h() - 400, 150, 200);
        player_chosen->set_model(model->player_chosen);

        player_points_label = new Fl_Box(20, Fl::h() - 80, 200, 40);
        player_points_label->labelsize(24);
        player_points_label->labelfont(FL_BOLD);

        npc_points_label = new Fl_Box(20, 20, 200, 40);
        npc_points_label->labelsize(24);
        npc_points_label->labelfont(FL_BOLD);
        
        turn_label = new Fl_Box(20, Fl::h() / 2, 200, 40);
        turn_label->labelsize(24);
        turn_label->labelfont(FL_BOLD);

        update_score_labels();

        main_win->end();
        main_win->show();
    }

    void update_score_labels() {
        if (player_points_label)
            player_points_label->copy_label(("Player: " + std::to_string(model->player_points)).c_str());

        if (npc_points_label)
            npc_points_label->copy_label(("NPC: " + std::to_string(model->npc_points)).c_str());

        if (turn_label)
            turn_label->copy_label(("Turn:" + std::to_string(model->turn)).c_str());

    }

    void on_player_card_clicked(int index) {
        if (index < 1 || index > NUM_CARDS) return;
        // index is 1..NUM_CARDS, convert to 0-based
        int i = index - 1;

        std::swap(model->player_cards[i], model->player_chosen);
        model->player_cards[i].caption = "Nothing";
        model->player_cards[i].atk = -1;
        player_cards[i]->set_model(model->player_cards[i]);
        player_chosen->set_model(model->player_chosen);
        
        model->calculate_turn();
        model->draw_card();
        
        for (int i = 0; i < NUM_CARDS; ++i)
        {
            player_cards[i]->set_model(model->player_cards[i]);
        }
        player_deck->set_model(model->player_deck_count_card);
        npc_deck->set_model(model->npc_deck_count_card);
        npc_chosen->set_model(model->npc_chosen);

        update_score_labels();

        main_win->redraw();
        if (model->game_finished)
        {
            fl_message(("Game finished! Player had: " + std::to_string(model->player_points) + "points, NPC had: " + std::to_string(model->npc_points)).c_str());
            back_cb(nullptr, this);
        }
    }

    void show_menu() {
        if (main_win) { delete main_win; main_win = nullptr; }
        menu_win->show();
    }

    static void card_click_cb(Fl_Widget* w, void* userdata) {
        auto* data = static_cast<CardCallbackData*>(w->user_data());
        if (data->controller->model->player_cards[data->index - 1].caption == "Nothing")
            return;
        data->controller->on_player_card_clicked(data->index);
    }
};
