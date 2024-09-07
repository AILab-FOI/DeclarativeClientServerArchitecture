import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import { Sadrzaj, TokenService, UserService } from '../../core';
import { ContentEditComponent, openModal } from '../../shared';
import { Dialog } from '@angular/cdk/dialog';
import { dohvati_sadrzaj, uredi_lekciju } from '../../../assets/pkg/client';
import { DatePipe } from '@angular/common';

@Component({
  selector: 'app-content',
  standalone: true,
  imports: [DatePipe],
  templateUrl: './content.component.html',
  styleUrl: './content.component.scss',
})
export class ContentComponent {
  public userService = inject(UserService);
  public tokenService = inject(TokenService);
  public content = signal<Sadrzaj>(undefined);
  public id = input.required<string>();
  public lastUpdated = computed(() => {
    return new Date(
      parseInt(this.content()?.vrijednost.vrijeme_kreiranja.toString()) * 1000,
    );
  });
  private dialog = inject(Dialog);
  constructor() {
    effect(() => {
      dohvati_sadrzaj(
        parseInt(this.id()),
        this.tokenService.accessToken(),
      ).then((sadrzaj) => {
        this.content.set(sadrzaj);
      });
    });
  }

  openEdit(): void {
    let ref = openModal<Sadrzaj>(this.dialog, {
      data: structuredClone(this.content()),
      tool: { view: ContentEditComponent },
      inputs: {},
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((content: Sadrzaj) => {
      if (content) {
        uredi_lekciju(
          content.id,
          content.naziv,
          content.vrijednost.sadrzaj,
          this.tokenService.accessToken(),
        ).then((res) => {
          dohvati_sadrzaj(res, this.tokenService.accessToken()).then(
            (sadrzaj) => {
              this.content.set(sadrzaj);
            },
          );
        });
      }
      ref.close();
    });
  }
}
